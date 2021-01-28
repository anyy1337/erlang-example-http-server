{application, spws,
[
    {description, "ShiftPlus web server"},
    {vsn, "1.0.0"},
    {modules, [spws_sup, tcp_listen, gateway_sup, client, parser_sup, http_parser]},
    {application, [kernel, stdlib]},
    {env, [{port, 8080}]},
    {mod, {spws, []}}
]}.
