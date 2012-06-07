require 'rubygems'
require 'erlectricity'
require 'active_record'
require 'yaml'
require 'rss/1.0'
require 'rss/2.0'
require 'open-uri'
require 'md5'

require 'rsscommon'

class RnFeedLink < ActiveRecord::Base ; end
class RvFeedLink < ActiveRecord::Base ; end

RvFeedLink.find( :all ).each do |feed|
  rss_line = RssLink.new({ 
                           :link => feed.link,
                           :md5 => MD5.new(feed.link + feed.name).to_s,
                           :name => feed.name,
                           :lang => feed.lang,
                           :src => "RV",
                           :desc => feed.desc,
                         })
  begin
    rss_line.save!
  rescue
  end
end

RnFeedLink.find( :all ).each do |feed|
  rss_line = RssLink.new({ 
                           :link => feed.link,
                           :md5 => MD5.new(feed.link + feed.name).to_s,
                           :name => feed.name,
                           :lang => feed.lang,
                           :src => "RN",
                           :desc => feed.desc,
                         })
  begin
    rss_line.save!
  rescue
  end
end
