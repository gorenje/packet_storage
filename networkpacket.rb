## helpers for constructing specific packet types.
def packet_ping(ttl,to,from)
  [:packet, # this tells erlang that the result is actually a packet
   Erl::List.new([
                  [:to,   Erl::List.new(to)],
                  [:from, Erl::List.new(from)],
                  [:type, :ping],
                  [:ttl,  ttl],
                 ])]
end

def packet_scrape(to,rss_link,partners,update_count)
  partners = partners.collect { |val| Erl::List.new(val) }
  to = Erl::List.new(to)
  [:packet,
  Erl::List.new([
                 [:type, :ruby_rss_scrape],
                 [:to, to],
                 [:from, Erl::List.new([0,0,0,0])],
                 [:partners, partners],
                 [:ttl, 50],
                 [:src, rss_link.link],
                 [:src_id, rss_link.id],
                 [:update_count, update_count],
                ])]
end

def packet_netstore(partners,data_id,data)
  partners = partners.collect { |val| Erl::List.new(val) }
  [:packet,
  Erl::List.new([
                 [:type, :netstore],
                 [:to, partners[1]],
                 [:from, partners[0]],
                 [:partners, Erl::List.new(partners[2..-1] + [partners[1],partners[0]])],
                 [:ttl, 20],
                 [:data_id, data_id],
                 [:data, data],
                ])]
end
