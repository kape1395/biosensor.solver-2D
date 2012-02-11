{application, bio_ers, [
    {description, "Erlang server for simulating biosensors"},
    {vsn, "0.7.0"},
    {modules, [bio_ers_app, bio_ers_sup, bio_ers_mgr]},
    {registered, [bio_ers]},
    {mod, {bio_ers_app, []}}
]}.

