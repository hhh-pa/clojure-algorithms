rootProject.name = "clojure-algorithms-bench"

// Use the CodSpeed fork of JMH (git submodule) in place of upstream JMH so
// benchmark results are collected and reported to CodSpeed.
includeBuild("../third-party/codspeed-jvm/jmh-fork") {
    dependencySubstitution {
        substitute(module("org.openjdk.jmh:jmh-core")).using(project(":jmh-core"))
        substitute(module("org.openjdk.jmh:jmh-generator-annprocess")).using(project(":jmh-generator-annprocess"))
        substitute(module("org.openjdk.jmh:jmh-generator-bytecode")).using(project(":jmh-generator-bytecode"))
        substitute(module("org.openjdk.jmh:jmh-generator-reflection")).using(project(":jmh-generator-reflection"))
        substitute(module("org.openjdk.jmh:jmh-generator-asm")).using(project(":jmh-generator-asm"))
    }
}
