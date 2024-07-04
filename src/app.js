var wasm;

window.onload = async function() {
    const viz = await Viz.instance();
    const output_region = document.getElementById("output-region");

    var default_engine_name;
    if (viz.engines.includes("dot")) {
        default_engine_name = "dot";
    }

    const engine_options = document.getElementById("input-engine");
    for (const engine_name of viz.engines) {
        const option = document.createElement("option");
        option.setAttribute("value", engine_name);
        if (engine_name == default_engine_name) {
            option.setAttribute("selected", "selected");
        }
        option.textContent = engine_name;
        engine_options.append(option);
    }

    const wasm_response = await fetch("bin/ReFsm.wasm");
    const wasm_bytes = await wasm_response.arrayBuffer();
    wasm = await WebAssembly.instantiate(wasm_bytes, {});
    const add = wasm.instance.exports.add;

    const {
        memory: wasm_memory,
        alloc_input_regex,
        generate_digraph,
        get_digraph_addr,
        get_digraph_length,
    } = wasm.instance.exports;

    var svg;
    document.getElementById("input-region").addEventListener("submit", function() {
        event.preventDefault();
        const form_data = new FormData(this);

        const input_regex = form_data.get("regex");
        const input_regex_addr = alloc_input_regex(input_regex.length);
        new Uint8Array(wasm_memory.buffer, input_regex_addr, input_regex.length)
            .set(new TextEncoder().encode(input_regex));

        generate_digraph();
        const digraph = new TextDecoder()
            .decode(new Uint8Array(wasm_memory.buffer, get_digraph_addr(), get_digraph_length()));

        if (svg != undefined) {
            output_region.removeChild(svg);
        }
        svg = viz.renderSVGElement(digraph, {
            engine: form_data.get("engine"),
        });
        svg.setAttribute("width", "100%");
        svg.setAttribute("height", "100%");
        output_region.appendChild(svg);
        panzoom(svg);
    })
};
