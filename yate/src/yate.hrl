-record(command,{type,                          % part of dict key
                 id,                            % part of dict key
                 success,                       % boolean success of operation
                 header,                        % one of the header
                                                % records below
                 keys}).

%% command header records
-record(message,{time,name,retvalue}).
-record(install_req,{priority,filter}).
-record(install_ans,{priority}).
-record(uninstall,{priority}).
-record(watch,{}).
-record(unwatch,{}).
-record(setlocal,{value}).
-record(output,{str}).
-record(connect,{role,type}).
