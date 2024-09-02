-module(server).

% elp:ignore W0013 (misspelled_attribute)
-mspec("?'neg'. !'ready'. ?integer. !integer.").

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Should fail since module is empty.
