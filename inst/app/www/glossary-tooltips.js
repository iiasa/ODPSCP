(function () {
  const DATA_NODE_ID = "odpscp-glossary-data";
  const TERM_CLASS = "odpscp-glossary-term";
  const SKIP_TAGS = new Set(["SCRIPT", "STYLE", "TEXTAREA", "OPTION", "BUTTON", "PRE", "CODE", "A", "TITLE"]);
  const SKIP_CLASSES = new Set([TERM_CLASS, "tooltip", "popover", "dataTables_wrapper", "selectize-input", "leaflet-control"]);

  let matcher = null;
  let glossaryDefinitions = new Map();
  let scheduled = false;
  let observer = null;
  let initialized = false;

  function escapeRegExp(value) {
    return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  }

  function getRoot() {
    return document.querySelector(".content-wrapper") || document.body;
  }

  function glossaryTooltipsEnabled() {
    const helpSwitch = document.getElementById("help_switch");
    if (!helpSwitch) {
      return true;
    }

    return Boolean(helpSwitch.checked);
  }

  function normalizeSpaces(value) {
    return value.replace(/\s+/g, " ").trim();
  }

  function loadGlossaryEntries() {
    const node = document.getElementById(DATA_NODE_ID);
    if (!node) {
      return [];
    }

    try {
      const parsed = JSON.parse(node.textContent || "[]");
      if (!Array.isArray(parsed)) {
        return [];
      }

      return parsed
        .map((entry) => ({
          match: normalizeSpaces(String(entry.match || "")),
          definition: normalizeSpaces(String(entry.definition || ""))
        }))
        .filter((entry) => entry.match.length > 0 && entry.definition.length > 0)
        .sort((left, right) => {
          if (right.match.length !== left.match.length) {
            return right.match.length - left.match.length;
          }

          return left.match.localeCompare(right.match);
        });
    } catch (error) {
      return [];
    }
  }

  function buildMatcher(entries) {
    glossaryDefinitions = new Map();
    const patterns = [];

    entries.forEach((entry) => {
      const key = entry.match.toLowerCase();
      if (glossaryDefinitions.has(key)) {
        return;
      }

      glossaryDefinitions.set(key, entry.definition);
      patterns.push(escapeRegExp(entry.match));
    });

    if (!patterns.length) {
      return null;
    }

    return new RegExp(`(^|[^A-Za-z0-9])(${patterns.join("|")})(?=$|[^A-Za-z0-9])`, "giu");
  }

  function shouldSkipNode(node) {
    if (!node || !node.nodeValue || !node.nodeValue.trim()) {
      return true;
    }

    let element = node.parentElement;
    if (!element) {
      return true;
    }

    while (element) {
      if (SKIP_TAGS.has(element.tagName)) {
        return true;
      }

      if (element.getAttribute && element.getAttribute("data-odpscp-glossary-skip") === "true") {
        return true;
      }

      if (element.classList) {
        for (const className of SKIP_CLASSES) {
          if (element.classList.contains(className)) {
            return true;
          }
        }
      }

      element = element.parentElement;
    }

    return false;
  }

  function createTermNode(text, definition) {
    const term = document.createElement("span");
    term.className = TERM_CLASS;
    term.textContent = text;
    term.setAttribute("tabindex", "0");
    term.setAttribute("data-toggle", "tooltip");
    term.setAttribute("data-placement", "top");
    term.setAttribute("title", definition);
    return term;
  }

  function annotateTextNode(node) {
    if (shouldSkipNode(node) || !matcher) {
      return;
    }

    const text = node.nodeValue;
    matcher.lastIndex = 0;
    let match = matcher.exec(text);

    if (!match) {
      return;
    }

    const fragment = document.createDocumentFragment();
    let lastIndex = 0;

    while (match) {
      const prefix = match[1] || "";
      const matchedText = match[2];
      const definition = glossaryDefinitions.get(matchedText.toLowerCase());
      const start = match.index;
      const termStart = start + prefix.length;
      const termEnd = termStart + matchedText.length;

      fragment.appendChild(document.createTextNode(text.slice(lastIndex, start)));

      if (prefix) {
        fragment.appendChild(document.createTextNode(prefix));
      }

      if (definition) {
        fragment.appendChild(createTermNode(matchedText, definition));
      } else {
        fragment.appendChild(document.createTextNode(matchedText));
      }

      lastIndex = termEnd;
      match = matcher.exec(text);
    }

    fragment.appendChild(document.createTextNode(text.slice(lastIndex)));
    node.parentNode.replaceChild(fragment, node);
  }

  function annotateGlossaryTerms(root) {
    if (!root || !matcher) {
      return;
    }

    const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT);
    const textNodes = [];

    while (walker.nextNode()) {
      textNodes.push(walker.currentNode);
    }

    textNodes.forEach(annotateTextNode);
  }

  function activateBootstrapTooltips(root) {
    if (!window.jQuery || !root) {
      return;
    }

    const enabled = glossaryTooltipsEnabled();
    document.body.classList.toggle("odpscp-glossary-disabled", !enabled);

    window.jQuery(root)
      .find(`.${TERM_CLASS}[data-toggle='tooltip']`)
      .each(function initializeTooltip() {
        const element = window.jQuery(this);
        if (!enabled) {
          if (element.data("bs.tooltip")) {
            element.tooltip("hide");
            element.tooltip("disable");
          }
          return;
        }

        if (!element.data("bs.tooltip")) {
          element.tooltip({
            boundary: "window",
            container: "body",
            trigger: "hover focus",
            template: '<div class="tooltip odpscp-glossary-tooltip" role="tooltip"><div class="arrow"></div><div class="tooltip-inner"></div></div>'
          });
        } else {
          element.tooltip("enable");
        }
      });
  }

  function applyGlossaryTooltips() {
    scheduled = false;
    const root = getRoot();

    annotateGlossaryTerms(root);
    activateBootstrapTooltips(root);
  }

  function scheduleApplyGlossaryTooltips() {
    if (scheduled) {
      return;
    }

    scheduled = true;
    window.requestAnimationFrame(applyGlossaryTooltips);
  }

  function initializeGlossaryTooltips() {
    if (initialized) {
      scheduleApplyGlossaryTooltips();
      return;
    }

    matcher = buildMatcher(loadGlossaryEntries());
    if (!matcher) {
      return;
    }

    initialized = true;
    scheduleApplyGlossaryTooltips();

    const root = getRoot();
    if (root) {
      observer = new MutationObserver(scheduleApplyGlossaryTooltips);
      observer.observe(root, { childList: true, subtree: true });
    }

    document.addEventListener("shiny:value", scheduleApplyGlossaryTooltips);
    document.addEventListener("change", function handleHelpSwitch(event) {
      if (event.target && event.target.id === "help_switch") {
        scheduleApplyGlossaryTooltips();
      }
    });

    if (window.jQuery) {
      window.jQuery(document).on(
        "shown.bs.tab shown.bs.collapse hidden.bs.tab hidden.bs.collapse",
        scheduleApplyGlossaryTooltips
      );
    }
  }

  document.addEventListener("DOMContentLoaded", initializeGlossaryTooltips);
  document.addEventListener("shiny:connected", initializeGlossaryTooltips);
})();