module Test.Utilities where


type Hash = String  -- FIXME: any need to get more complicated?

tickStub :: a
tickStub = error "tickstub"
  -- b/c I don't yet know how to tick things, :-)

trivTick :: a -> a
trivTick a = a

  
stub :: a
stub = error "stub"
