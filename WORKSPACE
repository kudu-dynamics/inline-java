workspace(name = "io_tweag_inline_java")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    sha256 = "34b29933ed5580e0203ca69948fe7b8126b75c4df02d1d43b4d9c1120656d548",
    strip_prefix = "rules_haskell-d162b8161a6cd26d40e08d37385c2d1272997a6c",
    urls = ["https://github.com/tweag/rules_haskell/archive/d162b8161a6cd26d40e08d37385c2d1272997a6c.tar.gz"],
)

load("@rules_haskell//haskell:repositories.bzl", "haskell_repositories")
haskell_repositories()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

nixpkgs_package(
    name = "alex",
    attribute_path = "haskellPackages.alex",
    repository = "@nixpkgs",
)

#nixpkgs_package(
#    name = "stack_ignore_global_hints",
#    attribute_path = "stack_ignore_global_hints",
#    repository = "@nixpkgs",
#)
#
#load("//:config_settings/setup.bzl", "config_settings")
#config_settings(name = "config_settings")
#load("@config_settings//:info.bzl", "ghc_version")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "Cabal",
        "aeson",
        "async",
        "base",
        "bytestring",
        "choice",
        "constraints",
        "containers",
        "criterion",
        "deepseq",
        "directory",
        "distributed-closure",
        "exceptions",
        "filemanip",
        "filepath",
        "ghc",
        "hspec",
        "inline-c",
        "language-java",
        "monad-logger",
        "mtl",
        "process",
        "QuickCheck",
        "quickcheck-text",
        "quickcheck-unicode",
        "split",
        "streaming",
        "template-haskell",
        "temporary",
        "text",
        "time",
        "vector",
        "unix",
        # dependencies of th-desugar
        "fail",
        "ghc-prim",
        "linear-base",
        "ordered-containers",
        "semigroups",
        "singletons",
        "singletons-base",
        "stm",
        "syb",
        "th-abstraction",
        "th-lift",
        "th-orphans",
        "transformers-compat",
        # dependencies of singletons
        "ghc-boot-th",
        "pretty",
        "transformers",
    ],
    extra_deps = { "zlib" : ["@zlib.dev//:zlib"] },
    local_snapshot = "//:snapshot-9.0.1.yaml",
    # stack = "@stack_ignore_global_hints//:bin/stack" if ghc_version == "9.0.1" else None,
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

nixpkgs_package(
    name = "glibc_locales",
    attribute_path = "glibcLocales",
    build_file_content = """
package(default_visibility = ["//visibility:public"])

filegroup(
    name = "locale-archive",
    srcs = ["lib/locale/locale-archive"],
)
""",
    repository = "@nixpkgs",
)

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc901",
    locale_archive = "@glibc_locales//:locale-archive",
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "9.0.1",
    compiler_flags = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
)

nixpkgs_package(
    name = "sed",
    attribute_path = "gnused",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "hspec-discover",
    attribute_path = "haskellPackages.hspec-discover",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "nixpkgs_zlib",
    attribute_path = "zlib",
    repository = "@nixpkgs",
)

nixpkgs_package(
    name = "zlib.dev",
    repository = "@nixpkgs",
    build_file_content = """
load("@rules_cc//cc:defs.bzl", "cc_library")
filegroup(
    name = "include",
    srcs = glob(["include/*.h"]),
    visibility = ["//visibility:public"],
)
cc_library(
    name = "zlib",
    srcs = ["@nixpkgs_zlib//:lib"],
    hdrs = [":include"],
    linkstatic = 1,
    strip_include_prefix = "include",
    visibility = ["//visibility:public"],
)
""",
)

nixpkgs_package(
    name = "openjdk",
    attribute_path = "openjdk11",
    repository = "@nixpkgs",
    build_file_content = """
filegroup(
    name = "bin",
    srcs = ["bin/javac"],
    visibility = ["//visibility:public"],
)

filegroup(
    name = "libjvm",
    srcs = select(
      { "@bazel_tools//src/conditions:darwin": ["lib/server/libjvm.dylib"],
        "@bazel_tools//src/conditions:linux_x86_64": ["lib/openjdk/lib/server/libjvm.so"],
      }),
    visibility = ["//visibility:public"],
)

cc_library(
    name = "lib",
    srcs = [":libjvm"],
    hdrs = ["include/jni.h", "include/jni_md.h"],
    strip_include_prefix = "include",
    linkstatic = 1,
    visibility = ["//visibility:public"],
)

# XXX Temporary workaround for
# https://github.com/bazelbuild/bazel/issues/8180.
genrule(
    name = "rpath",
    srcs = ["@openjdk//:libjvm"],
    cmd = "libjvm=$(location :libjvm); echo -rpath $$(dirname $$(realpath $$libjvm)) > $@",
    outs = ["openjdk_response_file"],
    visibility = ["//visibility:public"],
)
""",
)

RULES_JVM_EXTERNAL_TAG = "3.3"
RULES_JVM_EXTERNAL_SHA = "d85951a92c0908c80bd8551002d66cb23c3434409c814179c0ff026b53544dab"

http_archive(
    name = "rules_jvm_external",
    strip_prefix = "rules_jvm_external-%s" % RULES_JVM_EXTERNAL_TAG,
    sha256 = RULES_JVM_EXTERNAL_SHA,
    url = "https://github.com/bazelbuild/rules_jvm_external/archive/%s.zip" % RULES_JVM_EXTERNAL_TAG,
)

load("@rules_jvm_external//:defs.bzl", "maven_install")

maven_install(
    artifacts = [
        "org.apache.commons:commons-collections4:4.1",
        "com.wizzardo:http:0.3",
        "com.wizzardo:epoll:0.3.4",
        "com.wizzardo:reactive-pg-client:0.10.2.1",
        "com.wizzardo.tools:tools-collections:0.23",
        "com.wizzardo.tools:tools-interfaces:0.23",
    ],
    repositories = [
        "https://maven.google.com",
        "https://repo1.maven.org/maven2",
    ],
)
