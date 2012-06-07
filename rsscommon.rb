ActiveRecord::Base.establish_connection({
                                          :adapter => "mysql", 
                                          :database => 'erlang',
                                          :encoding => 'utf8',
                                          :username => 'root',
                                          :password => '',
                                          :host => 'localhost'
                                        })

class RssItem < ActiveRecord::Base
  belongs_to :rss_link
end

class RssLink < ActiveRecord::Base
  has_many :rss_link_status
  has_many :rss_items
end

class RssLinkStatus < ActiveRecord::Base
  set_table_name :rss_link_stati
  belongs_to :rss_link
end

# rerieve and pass the rss feed returning status information about the feed.
def check_feed(feed, &block)
  begin
    rss = RSS::Parser.parse(open(feed.link) { |s| s.read }, false)
    if rss.nil?
      [:ok, 1, 0]
    else
      cnt = rss.items.size
      if block_given?
        cnt = 0
        rss.items.each do |item|
          cnt += yield(feed,item)
        end
      end
      [:ok, 1, cnt]
    end
  rescue OpenURI::HTTPError 
    [:error, 3, "HTTPError: #{$!.message}"] # 404 Not Found
  rescue RSS::NotWellFormedError
    [:error, 4, "Not Well Formed RSS: #{$!.message}"]
  rescue Errno::ETIMEDOUT
    [:error, 2, "Timedout"]
  rescue Errno::ENOENT
    [:error, 5, "No entity found: #{$!.message}"]
  rescue SocketError
    [:error, 5, "Couldn't connect #{$!.message}"]
  rescue URI::InvalidURIError
    [:error, 4, "Invalid URI, #{$!.message}"]
  rescue NoMethodError
    [:error, 4, "Invalid RSS? #{$!.message}"]
  rescue OpenSSL::SSL::SSLError
    [:error, 5, "SSLError: #{$!.class.name}"]
  rescue
    [:error, 9, "unknown error: [#{$!.class.name}]: #{$!.message}"]
  end
end
