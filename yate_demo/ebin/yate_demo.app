{application, yate_demo,
 [{description, "Yate demo module"},
  {vsn,"0.0"},
  {modules, [
	     yate_demo_app,
	     yate_demo_sup,
	     yate_demo,
	     yate_demo_call,
	     yate_clock
	    ]},
  {registered, [
		yate_demo_sup,
		yate_demo
	       ]},
  {mod, {yate_demo_app, []}},
  {env, []},
  {applications,
        [kernel, stdlib, yate]}]}.