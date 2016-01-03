defmodule Sample do
  def test() do
    x = Poison.decode!(~s({"people": [{"name": "Devin Torres", "age": 27}]}))
    IO.inspect x
    y = Poison.encode!(x)
    IO.inspect y
    :ok
  end
end
