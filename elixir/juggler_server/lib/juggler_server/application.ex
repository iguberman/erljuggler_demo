defmodule JugglerServer.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    IO.write("MODULE: #{inspect(__MODULE__)}\n")
    children = [
      %{id: JugglerServer.RequestDispatcher, start: {RequestDispatcher, :start_link, []}},
      {DynamicSupervisor, name: JugglerServer.RequestHandlerSupervisor, strategy: :one_for_one, restart: :transient, shutdown: :brutal_kill}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: JugglerServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
