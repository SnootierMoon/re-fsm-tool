var wasm;

const wasm_imports = {
    env: {
        console_print: (lvl, addr, len) => {
            const msg = new TextDecoder()
                .decode(new Uint8Array(wasm.instance.exports.memory.buffer, addr, len));
            if (lvl === 3) {
                console.error("WASM: " + msg);
            } else if (lvl === 2) {
                console.warn("WASM: " + msg);
            } else if (lvl === 1) {
                console.info("WASM: " + msg);
            } else if (lvl === 0) {
                console.debug("WASM: " + msg);
            } else {
                console.log("WASM: " + msg);
            }
        }
    },
};

window.onload = async function() {
    const viz = await Viz.instance();
    const output_region = document.getElementById("output-region");

    var default_engine_name;
    if (viz.engines.includes("dot")) {
        default_engine_name = "dot";
    }


    const wasm_response = await fetch("ReFsm.wasm");
    const wasm_bytes = await wasm_response.arrayBuffer();
    wasm = await WebAssembly.instantiate(wasm_bytes, wasm_imports);

    const {
        memory: wasm_memory,
        alloc_input_regex,
        generate_digraph,
        get_digraph_addr,
        get_digraph_length,
    } = wasm.instance.exports;

    var svg;

    const input = document.querySelector("input");
    input.addEventListener("input", function() {
        const input_regex = input.value;
        const input_regex_addr = alloc_input_regex(input_regex.length);
        if (0 <= input_regex_addr && 
            input_regex_addr + input_regex.length <= wasm_memory.buffer.byteLength) {
            new Uint8Array(wasm_memory.buffer, input_regex_addr, input_regex.length)
                .set(new TextEncoder().encode(input_regex));
        }

        if (generate_digraph() == 0) {
            input.classList.add("error");
            return;
        } else {
            input.classList.remove("error");
        }

        const digraph = new TextDecoder()
            .decode(new Uint8Array(wasm_memory.buffer, get_digraph_addr(), get_digraph_length()));

        console.log(digraph);

        var old_svg = svg;
        svg = viz.renderSVGElement(digraph, {
            engine: "dot",
        });
        if (old_svg != undefined) {
            output_region.removeChild(old_svg);
        }
        output_region.appendChild(svg);
        panzoom(svg);
    })
};
