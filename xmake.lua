---@diagnostic disable: undefined-global, undefined-field

target("rtl", function()
    set_kind("phony")
    on_run(function()
        local build_dir = path.join("build", "rtl")
        local rtl_opts = {
            "-i",
            "chiselTemplate.runMain",
            "GenerateVerilog",
            "--target",
            "systemverilog",
            "--split-verilog",
            "-td",
            build_dir
        }
        os.tryrm(build_dir)
        os.execv("mill", rtl_opts)
    end)
end)

target("comp", function()
    set_kind("phony")
    on_run(function()
        os.execv("mill", { "-i", "chiselTemplate.compile" })
    end)
end)

target("fmt", function()
    set_kind("phony")
    on_run(function()
        os.execv("mill", { "-i", "chiselTemplate.reformat" })
    end)
end)

target("clean", function()
    set_kind("phony")
    on_run(function()
        os.rmdir(path.join("build"))
    end)
end)

target("init", function()
    set_kind("phony")
    set_default(false)
    local function isempty(v)
        return v == nil or v == ""
    end
    local default_proxy = os.getenv("default_proxy_LAN")
    local http          = os.getenv("http_proxy")
    local https         = os.getenv("https_proxy")
    local isProxyEmpty  = isempty(http) or isempty(https)
    local autoSetProxy  = false

    before_run(function() -- Check whether the environment variables about system proxy is OK or not.
        if (isProxyEmpty) then
            cprint("${yellow underline}[WARNING]${clear} http_proxy and https_proxy have not been set.")
            if (isempty(default_proxy)) then
                cprint("${red underline}[SEVERE]${clear} There are no proxy set. Initialization operation failed.")
                local msg = format("Initialization failed")
                raise(msg)
            else
                autoSetProxy = true
            end
        end
    end)

    on_run(function()
        if (autoSetProxy) then
            local envs          = {}
            envs["http_proxy"]  = default_proxy
            envs["https_proxy"] = default_proxy
            os.addenvs(envs)
            cprint("${green underline}[INFO] Default proxy has been set. Proxy has been configured automatically.${clear}")
        end
        cprint("${green underline}[INFO] Updating submodules in this repo... This may take a few seconds.${clear}")
        os.cd(os.scriptdir())
        os.exec("git submodule update --init")
    end)
end)
