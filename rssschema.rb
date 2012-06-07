require 'rubygems'
require 'active_record'

require 'rsscommon'

ActiveRecord::Schema.define(:version => 20080902145655) do

  create_table "rss_links", :force => true do |t|
    t.string "link", :null => false, :limit => 4096
    t.string "md5", :null => false, :limit => 32
    t.string "name", :limit => 2048
    t.string "lang", :limit => 32
    t.string "src", :limit => 32
    t.string "tags", :limit => 756
    t.text "desc"
    t.integer "last_status", :default => 0
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "rss_links", ["md5"], :name => "rss_links_md5", :unique => true
  add_index "rss_links", ["src"], :name => "rss_links_src"
  add_index "rss_links", ["tags"], :name => "rss_links_tags"
  add_index "rss_links", ["last_status"], :name => "rss_links_last_status"

  create_table "rss_link_stati", :force => true do |t|
    t.integer "rss_link_id", :null => false
    t.integer "update_count", :null => false
    t.integer "status", :null => false
    t.integer "item_count", :default => 0, :null => false
    t.text "error_msg"
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "rss_link_stati", ["status"], :name => "rss_link_stati_status"
  add_index "rss_link_stati", ["update_count"], :name => "rss_link_stati_update_count"
  add_index "rss_link_stati", ["rss_link_id"], :name => "rss_link_stati_rss_link_id"
  add_index "rss_link_stati", ["item_count"], :name => "rss_link_stati_item_count"
  add_index "rss_link_stati", ["update_count", "rss_link_id"], :name => "rss_link_stati_update_count_rss_link", :unique => true
  
  create_table "rss_items", :force => true do |t|
    t.integer "rss_link_id"
    t.string "link", :limit => 4096, :null => false
    t.string "md5", :limit => 32, :null => false
    t.string "title", :limit => 2048
    t.text "desc"
    t.string "tags", :limit => 756
    t.datetime "created_at", :null => false
    t.datetime "updated_at", :null => false
  end

  add_index "rss_items", ["rss_link_id"], :name => "rss_items_rss_link_id"
  add_index "rss_items", ["md5"], :name => "rss_items_md5", :unique => true
  add_index "rss_items", ["tags"], :name => "rss_items_tags"
end
