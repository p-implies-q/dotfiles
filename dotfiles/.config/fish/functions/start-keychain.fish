function start-keychain
  eval (keychain --eval --agents ssh -Q --quiet id_rsa --nogui)
end
