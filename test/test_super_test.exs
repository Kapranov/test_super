defmodule TestSuperTest do
  use ExUnit.Case
  doctest TestSuper

  test "the truth" do
    assert 1 + 1 == 2
  end
end
