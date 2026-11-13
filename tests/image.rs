//! Build the Clojure image (clj/init.clj) with the binary, then start
//! from it.

use std::process::Command;

#[test]
fn boot_save_and_load_image() {
    let exe = env!("CARGO_BIN_EXE_corten");
    let root = env!("CARGO_MANIFEST_DIR");
    let img = std::env::temp_dir().join(format!("corten-test-{}.img", std::process::id()));
    let img = img.to_str().unwrap();
    // Collect often while building and using the image.
    let out = Command::new(exe)
        .current_dir(root)
        .env("CORTEN_GC_THRESHOLD", "256k")
        .args(["--boot", "clj/init.clj", "--save", img])
        .output()
        .unwrap();
    assert!(out.status.success(), "boot failed: {}", String::from_utf8_lossy(&out.stderr));

    let start = std::time::Instant::now();
    let out = Command::new(exe)
        .env("CORTEN_GC_THRESHOLD", "64k")
        .args([
            "--image",
            img,
            "-e",
            "(defn fact [n] (if (< n 2) 1 (* n (fact (- n 1)))))",
            "-e",
            "[(ns-name *ns*) (fact 15) (str 'ok) (frequencies \"abca\")]",
        ])
        .output()
        .unwrap();
    let elapsed = start.elapsed();
    let _ = std::fs::remove_file(img);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(out.status.success(), "image run failed: {}", String::from_utf8_lossy(&out.stderr));
    assert_eq!(stdout, "#'user/fact\n[user 1307674368000 \"ok\" {\\a 2, \\b 1, \\c 1}]\n");
    assert!(elapsed.as_secs_f64() < 1.0, "image startup took {elapsed:?}");
}
