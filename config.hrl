-define(SOCK_OPTS, [binary, {active, false},
                            {sndbuf,4194304},
                            {recbuf,4194304},
                            {packet, raw}]).
-define(PORT, 1337).
-define(TIMEOUT, 30000).
-define(CHUNK_SIZE, 200000).
% -define(HOST, "128.199.56.84").
-define(HOST, "localhost").
