require 'rubygems'
require 'erlectricity'
require 'active_record'
require 'yaml'
require 'rss/1.0'
require 'rss/2.0'
require 'open-uri'
require 'md5'
require 'ostruct'

require 'rsscommon'
require 'networkpacket'

## helper for extracting specific information from
## packet data.
class Array
  def obtain(key)
    rs = (self.detect { |obj| !obj.nil? and obj[0] == key })
    rs.nil? ? nil : rs[1]
  end
end

# allow us to use hashes but not really needed anymore
# since i discovered the Erl::List.new construct.
module Erlectricity
  class Encoder
    def write_any_raw_extended(obj)
      case obj
      when Hash then write_tuple(obj.to_a)
      else
        write_any_raw_old(obj)
      end
    end
    alias_method :write_any_raw_old, :write_any_raw
    alias_method :write_any_raw, :write_any_raw_extended
  end
end

# all parameters should be passed in via the environment and not command line
# this makes linking into the erlang process much easier
@node_name = ENV["NODE_NAME"] || "Unknown"

LOG = File.open("networkrubyengine-#{@node_name}.log", "a")
(LOG << "starting listener\n").flush

## md5's of the links that this process has already seen. this
## is used to limit the number of AR creates we do.
links_stored = []

receive do |f|
  ## this and the next when can be basically be used for testing
  ## are not longer used directly, instead specific packets have
  ## been defined and are used.
  f.when( :execute, String ) do |value|
    (LOG << "Got command message: #{value}\n").flush
    f.send! :ok, :command_received
    result_code, return_value = begin
                                  [:ruby_results, eval("#{value}\n")]
                                rescue SyntaxError => error
                                  (LOG << error.backtrace.join("\n")).flush
                                  [:bad, error.to_s]
                                rescue
                                  (LOG << $!.message).flush
                                  (LOG << $!.backtrace.join("\n")).flush
                                  [:bad, "Exception rasies"]
                                end
    (LOG << "Sending response: [#{result_code}] [#{return_value}]\n").flush
    f.send! result_code, return_value
    f.receive_loop
  end

  ## catch all for the execute message
  f.when( :execute, Any ) do |value|
    (LOG << "Got execute command message with Any: #{value}, #{value.class.name} ignoring\n").flush
    f.send! :bad, :command_received_unknown
    f.receive_loop
  end

  ## received some data from network, need to store this
  ## this is the handler for netstore data sent to ruby.
  ## we don't need to request the data, this is done for
  ## us but we need to be able to handle it.
  f.when( :packet, :netstore_data, Any ) do |value|
    data_id = value.obtain(:data_id)

    case data_id[0]
      when :rss_link_status
      result = value.obtain(:data)[0][1]
      update_count = value.obtain(:data)[1][1]
      begin
        RssLinkStatus.create({ 
                               :rss_link_id => data_id[1].to_i,
                               :update_count => update_count,
                               :status => result[1],
                               :item_count => (result[0] == :ok ? result[2] : -1),
                               :error_msg => (result[0] == :error ? result[2] : nil),
                             })
        RssLink.update( data_id[1].to_i, { :last_status => result[1] })
      rescue
      end

      when :link
        data = (value.obtain(:data))[0][1]
        rss_link_id = (value.obtain(:data))[1][1]
        obj = YAML.load(StringIO.new(data))
        md5 = data_id[1].to_s
        (LOG << "NDPkt: #{md5} ").flush
        case obj
          when RSS::Rss::Channel::Item
            begin
              ## md5 is unique key so this insert can definitely fail!
              unless links_stored.include?(md5)
                RssItem.create( :md5 => md5,
                                :title => obj.title,
                                :link => obj.link,
                                :desc => obj.description,
                                :rss_link_id => rss_link_id )
                links_stored << md5
                (LOG << "yes\n").flush
              else
                (LOG << "no\n").flush
              end
            rescue
              (LOG << $!.message).flush
              (LOG << "no - db error\n").flush
            end
        end
      else
      (LOG << 'Unknown Netstore DataId: #{data_id[0]}').flush
    end

    f.receive_loop
  end

  ## this is a request for the ruby process to generate a netstore packet
  ## to the partners provided by original packet
  f.when( :packet, :ruby_netstore, Any ) do |value|
    (LOG << "Netstore Packet\n").flush
    partners = value.obtain(:partners)
    query = value.obtain(:src)
    query = query.pack("C"*query.length)
    f.send!(:ruby_results,packet_netstore(partners, [query], [eval(query).to_yaml]))
    f.receive_loop
  end

  ## this the scraper. it retrieves the rss feed and for each item
  ## found, a netstore packet is generated. this is then eventually
  ## picked up by another ruby process and stored in the database
  f.when( :packet, :ruby_rss_scrape, Any ) do |value|
    (LOG << "Rss scrape\n").flush
    feed_id = value.obtain(:src_id)
    update_count = value.obtain(:update_count)
    query = value.obtain(:src)
    query = case query
            when Array
              query.pack("C"*query.length)
            when String
              query
            else 
              query
            end

    # in fact we don't have to do this, the ruby process is
    # respawned if it dies. So in fact we can program the "best case"
    # in ruby and let erlang restart if we fail
    status = check_feed(OpenStruct.new({ :link => query,
                                         :id => feed_id,
                                       })) do |feed, item|
      f.send!(:ruby_results, packet_netstore(value.obtain(:partners), [:link,MD5.new(item.link+item.title).to_s], 
                                             Erl::List.new([ [:item, item.to_yaml], [:rss_link_id, feed_id]])))
      1
    end
    
    f.send!(:ruby_results, packet_netstore(value.obtain(:partners), 
                                           [:rss_link_status, feed_id],
                                           Erl::List.new([ [:status, status], [:update_count, update_count]])))
    f.receive_loop
  end

  ## seed the rss scrape with new urls. this is just sends out
  ## rss_scrape packets to the scrapers provided by the original
  ## packet.
  f.when( :packet, :ruby_rss_seeder, Any ) do |value|
    (LOG << "Rss url seeder\n").flush
    partners = value.obtain(:partners)
    scrapers = value.obtain(:sources)
    start_idx = value.obtain(:start_at)
    update_count = value.obtain(:update_count)
    limit = value.obtain(:count)

    RssLink.find( :all, :conditions => ['id > ?',start_idx], :limit => limit).each do |feed|
      head = scrapers.shift
      f.send!(:ruby_results, packet_scrape(head,feed,partners,update_count))
      scrapers += [head]
    end
    f.receive_loop
  end

  ## this dummy picks up everything that wasn't picked before here!
  f.when( Any ) do |value|
    (LOG << "Got command message with Any: #{value}, #{value.class.name} ignoring\n").flush
    f.receive_loop
  end
end

(LOG << "Exiting\n").flush
