defmodule JugglerServerTest do
  use ExUnit.Case
  doctest JugglerServer

  test "greets the world" do
    assert JugglerServer.hello() == :world
  end
end
