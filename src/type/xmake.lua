add_requires("stx")

target("type")
    set_kind("shared")
    add_files("src/**.cpp")
    add_includedirs("include")
    add_packages("stx")