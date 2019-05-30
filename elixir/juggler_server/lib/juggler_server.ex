defmodule JugglerServer do
  @moduledoc false

  def accept(num_requests) do
    for x <- :lists.seq(1, num_requests) do
      GenServer.cast(RequestDispatcher, x)
    end
    send(RequestDispatcher, {:request_stats, num_requests})
  end

end
