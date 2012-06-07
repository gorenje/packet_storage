require 'rubygems'
require 'erlectricity'

LOG = File.open("simplereceiver.log", "w")

LOG << "starting listener\n"
LOG.flush

receive do |f|

  # this requires sending messages in order before expecting a result.
  # i.e. you need to send the ruby process two messages before doing a 
  # receive on the erlang side. this sucks.
  # you can't even use a second shell to send the ruby process the second
  # message, you need to do it from the first.
  f.when( :one, Any ) do |value|
    LOG << "One: #{value}\n"
    LOG.flush
    receive do |g|
      g.when( :two, Any) do |value2|
        LOG <<  "Two: #{value2}\n"
        LOG.flush
      end
    end
    f.send! :ok, :received_one
    f.receive_loop
  end

  # using a specific type for the argument is a major pain!
  # Always have a Any clause to catch anything unexpected.
  # i found that using just a specific type just hangs the ruby process
  # because it matches the symbol but not the value type (when the value
  # sent is not of the required type) ... strange.
  f.when( :three, String ) do |value|
    LOG <<  "Three: (String) #{value}\n"
    LOG.flush
    f.send! :ok, :received_three
    f.receive_loop
  end

  f.when( :three, Any ) do |value|
    LOG <<  "Three: (Any) #{value} (#{value.class.name})\n"
    LOG.flush
    f.send! :ok, :received_three
    f.receive_loop
  end
  
  # always have a catch-all!! Syntax is kinda weird (imho f.else would
  # be better) but it definitely catches everything
  f.when(Any) do 
    LOG <<  "Unknown received\n"
    LOG.flush
    f.send! :ok, :unknown_received
  end

end

LOG << "Exiting"
LOG.flush
