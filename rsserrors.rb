require 'rubygems'
require 'erlectricity'
require 'active_record'
require 'yaml'
require 'rss/1.0'
require 'rss/2.0'
require 'open-uri'
require 'md5'

require 'rsscommon'

start_idx = 370
limit = 10

RssLink.find( :all, :conditions => ['id > ?',start_idx], :limit => limit).each_with_index do |feed, idx|
  result = check_feed(feed) do |feed,item|
    rss_item = RssItem.new({ 
                             :rss_link => feed,
                             :title => item.title,
                             :desc => item.description,
                             :link => item.link,
                             :md5 => MD5.new(item.link + item.title).to_s,
                           })
    begin
      rss_item.save!
      1
    rescue
      0
    end  
  end

  feed.last_status = result[1]
  feed.save
  rss_status = RssLinkStatus.new({ 
                                   :rss_link => feed,
                                   :status => result[1],
                                   :item_count => (result[0] == :ok ? result[2] : -1),
                                   :error_msg => (result[0] == :error ? result[2] : nil),
                                 })
  rss_status.save
end
