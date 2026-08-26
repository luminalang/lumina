// Populate the sidebar
//
// This is a script, and not included directly in the page, to control the total size of the book.
// The TOC contains an entry for each page, so if each page includes a copy of the TOC,
// the total size of the page becomes O(n**2).
class MDBookSidebarScrollbox extends HTMLElement {
    constructor() {
        super();
    }
    connectedCallback() {
        this.innerHTML = '<ol class="chapter"><li class="chapter-item expanded affix "><a href="index.html">The Lumina Programming Language</a></li><li class="chapter-item expanded affix "><li class="part-title">Getting Started</li><li class="chapter-item expanded "><a href="getting-started/installation.html"><strong aria-hidden="true">1.</strong> Installation</a></li><li class="chapter-item expanded "><a href="getting-started/compiling-and-running.html"><strong aria-hidden="true">2.</strong> Compiling &amp; Running Programs</a></li><li class="chapter-item expanded "><a href="getting-started/create-project.html"><strong aria-hidden="true">3.</strong> Creating a Lumina Project</a></li><li class="chapter-item expanded affix "><li class="part-title">Features</li><li class="chapter-item expanded "><a href="features/functions.html"><strong aria-hidden="true">4.</strong> Functions</a></li><li class="chapter-item expanded "><a href="features/types.html"><strong aria-hidden="true">5.</strong> Types</a></li><li class="chapter-item expanded "><a href="features/let-and-do.html"><strong aria-hidden="true">6.</strong> let &amp; do</a></li><li class="chapter-item expanded "><a href="features/matching.html"><strong aria-hidden="true">7.</strong> Pattern Matching &amp; Conditionals</a></li><li class="chapter-item expanded "><a href="features/partial.html"><strong aria-hidden="true">8.</strong> Closures &amp; Partial Application</a></li><li class="chapter-item expanded "><a href="features/list-and-strings.html"><strong aria-hidden="true">9.</strong> Lists &amp; Strings</a></li><li class="chapter-item expanded "><a href="features/val.html"><strong aria-hidden="true">10.</strong> Declaring Global Values</a></li><li class="chapter-item expanded "><a href="features/modules.html"><strong aria-hidden="true">11.</strong> Module System</a></li><li class="chapter-item expanded "><a href="features/generics-and-traits.html"><strong aria-hidden="true">12.</strong> Generics &amp; Traits</a></li><li class="chapter-item expanded "><a href="features/pipes.html"><strong aria-hidden="true">13.</strong> Pipes (dot notation)</a></li><li class="chapter-item expanded "><a href="features/attributes.html"><strong aria-hidden="true">14.</strong> Attributes</a></li><li class="chapter-item expanded affix "><li class="part-title">Advanced Features</li><li class="chapter-item expanded "><a href="features/pointers.html"><strong aria-hidden="true">15.</strong> Pointer Arithmetics</a></li><li class="chapter-item expanded "><a href="features/ffi.html"><strong aria-hidden="true">16.</strong> Calling C functions via FFI</a></li><li class="chapter-item expanded "><a href="features/nested-records.html"><strong aria-hidden="true">17.</strong> Updating Nested Records</a></li><li class="chapter-item expanded affix "><li class="part-title">Standard Library</li><li class="chapter-item expanded "><a href="std/targets.html"><strong aria-hidden="true">18.</strong> Available Targets</a></li><li class="chapter-item expanded "><a href="std/io.html"><strong aria-hidden="true">19.</strong> IO and the file system</a></li><li class="chapter-item expanded "><a href="std/lists.html"><strong aria-hidden="true">20.</strong> List and its Sub-types</a></li></ol>';
        // Set the current, active page, and reveal it if it's hidden
        let current_page = document.location.href.toString().split("#")[0];
        if (current_page.endsWith("/")) {
            current_page += "index.html";
        }
        var links = Array.prototype.slice.call(this.querySelectorAll("a"));
        var l = links.length;
        for (var i = 0; i < l; ++i) {
            var link = links[i];
            var href = link.getAttribute("href");
            if (href && !href.startsWith("#") && !/^(?:[a-z+]+:)?\/\//.test(href)) {
                link.href = path_to_root + href;
            }
            // The "index" page is supposed to alias the first chapter in the book.
            if (link.href === current_page || (i === 0 && path_to_root === "" && current_page.endsWith("/index.html"))) {
                link.classList.add("active");
                var parent = link.parentElement;
                if (parent && parent.classList.contains("chapter-item")) {
                    parent.classList.add("expanded");
                }
                while (parent) {
                    if (parent.tagName === "LI" && parent.previousElementSibling) {
                        if (parent.previousElementSibling.classList.contains("chapter-item")) {
                            parent.previousElementSibling.classList.add("expanded");
                        }
                    }
                    parent = parent.parentElement;
                }
            }
        }
        // Track and set sidebar scroll position
        this.addEventListener('click', function(e) {
            if (e.target.tagName === 'A') {
                sessionStorage.setItem('sidebar-scroll', this.scrollTop);
            }
        }, { passive: true });
        var sidebarScrollTop = sessionStorage.getItem('sidebar-scroll');
        sessionStorage.removeItem('sidebar-scroll');
        if (sidebarScrollTop) {
            // preserve sidebar scroll position when navigating via links within sidebar
            this.scrollTop = sidebarScrollTop;
        } else {
            // scroll sidebar to current active section when navigating via "next/previous chapter" buttons
            var activeSection = document.querySelector('#sidebar .active');
            if (activeSection) {
                activeSection.scrollIntoView({ block: 'center' });
            }
        }
        // Toggle buttons
        var sidebarAnchorToggles = document.querySelectorAll('#sidebar a.toggle');
        function toggleSection(ev) {
            ev.currentTarget.parentElement.classList.toggle('expanded');
        }
        Array.from(sidebarAnchorToggles).forEach(function (el) {
            el.addEventListener('click', toggleSection);
        });
    }
}
window.customElements.define("mdbook-sidebar-scrollbox", MDBookSidebarScrollbox);
