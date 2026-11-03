plugins {
    java
    id("me.champeau.jmh") version "0.7.3"
}

repositories {
    mavenCentral()
}

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

sourceSets {
    named("jmh") {
        // Clojure sources are loaded (and compiled) at runtime from the
        // classpath, so the library and the benchmark fixtures are shipped as
        // resources of the benchmark jar.
        resources {
            srcDir("../src")
            srcDir("../resources")
            srcDir("src/jmh/clojure")
        }
    }
}

dependencies {
    // Keep in sync with deps.edn
    jmh("org.clojure:clojure:1.12.0")
}

jmh {
    // Resolved to the CodSpeed JMH fork through the composite build in
    // settings.gradle.kts.
    jmhVersion.set("0.2.1")

    // Warmup/measurement/fork settings are declared with annotations on the
    // benchmark classes so they also apply when running the jar directly.
    forceGC.set(true)

    // Optional benchmark filter, e.g. `./gradlew jmh -Pbench=Sorts`
    project.findProperty("bench")?.let { includes.set(listOf(it.toString())) }
}
