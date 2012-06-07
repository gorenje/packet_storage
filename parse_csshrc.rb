hsh = { }
STDIN.read.each do |line|
  if ( mth = line.match(/([^[:space:]]*)[[:space:]]*=[[:space:]]*(.*)/) ) 
    hosts = mth[2]
    hsh.each do |k,v|
      hosts.gsub!(/^#{k} /, " #{v} ")
      hosts.gsub!(/ #{k} /, " #{v} ")
      hosts.gsub!(/ #{k}$/, " #{v} ")
    end
    hsh[mth[1]] = hosts
  end
end
puts hsh["planetlab"]

