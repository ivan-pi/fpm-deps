module fpmdeps_html

    implicit none
    private

    public :: print_mermaid_html

contains

    subroutine print_mermaid_html(html_unit,package_name,print_mermaid_graph)
        integer, intent(in) :: html_unit
        character(len=*), intent(in) :: package_name
        interface
            subroutine print_mermaid_graph(html_unit,name)
                integer, intent(in) :: html_unit
                character(len=*), intent(in) :: name
            end subroutine
        end interface

        character(len=:), allocatable :: ht, bt

        ht = "<title>"//package_name//" dependency graph</title>"
        bt = "<h1>"//package_name//" dependency graph</h1>"

        write(html_unit,'(*(A,:,/))') &
'<!DOCTYPE html>', &
'<html lang="en">', &
'<head>', &
'    <meta charset="UTF-8">', &
'    <meta name="viewport" content="width=device-width, initial-scale=1.0">', &
ht, &
'    <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>', &
'    <style>', &
'      html {', &
'        min-height: 100%;', &
'        position: relative;', &
'      }', &
'      body {', &
'        font-family: Arial, sans-serif;', &
'        margin: 0;', &
'        padding: 20px;', &
'        padding-bottom: 50px; /* extra space */', &
'      }', &
'      footer {', &
'        position: absolute;', &
'        bottom: 0;', &
'        left: 0;', &
'        width: 100%;', &
'        text-align: left;', &
'        font-size: 0.9em;', &
'        color: #555;', &
'        padding: 10px;', &
'      }', &
'    </style>', &
'</head>', &
'<body>', &
'  <div class="diagram-container" id="diagram-container">', &
'  <pre class="mermaid">'

call print_mermaid_graph(html_unit,package_name)

write(html_unit,'(*(A,/))') &
'  </pre>', &
'  </div>', &
'  <script>', &
'    window.callback = function () {', &
'      alert("A callback was triggered");', &
'    };', &
'    const config = {', &
'      startOnLoad: true,', &
'      flowchart: { useMaxWidth: true, htmlLabels: true, curve: "cardinal" },', &
'      securityLevel: "loose",', &
'    };', &
'    mermaid.initialize(config);', &
'  </script>', &
'  <footer>', &
'    Generated using <a href="https://github.com/ivan-pi/fpm-deps">fpm-deps</a>', &
'  </footer>', &
'</body>', &
'</html>'

    end subroutine

end module
