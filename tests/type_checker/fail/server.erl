-module(server).

% elp:ignore W0013 (misspelled_attribute)
-mspec("?'neg'. !'ready'. <'state'>. ?integer. !integer. end").

% A simple static dual check
% By Emil Kristensen, ITU 2024

% Should fail since module is empty.
