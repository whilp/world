def home_files():
    native.http_file(
        name = "flattened_dark",
	url = "https://raw.githubusercontent.com/romainl/flattened/891532cd94b649bc668e4a5571195444e89a6f9a/colors/flattened_dark.vim",
	#sha256 = "bdb159bbb494aaa7163ba3a412a711fbe45cc077afa97f737ce836a1d7511bef",
	sha256 = "fd57dd3d13e385418126cafcc4149b9a6dc971247822c303b59849e6f52f78ef",
	)

    native.http_file(
        name = "flattened_light",
	url = "https://raw.githubusercontent.com/romainl/flattened/891532cd94b649bc668e4a5571195444e89a6f9a/colors/flattened_light.vim",
	sha256 = "0203504fc0a9a507cbe8192fc55dd5f94520cb1f43858e62397a852133d3b1fd",
    )
