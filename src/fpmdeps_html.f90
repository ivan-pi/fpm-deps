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

        write(html_unit,'(*(A,/))') &
'<!DOCTYPE html>', &
'<html lang="en">', &
'<head>', &
'    <meta charset="UTF-8">', &
'    <meta name="viewport" content="width=device-width, initial-scale=1.0">', &
ht, &
'    <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>', &
'    <style>', &
'        body {', &
'            font-family: Arial, sans-serif;', &
'            padding: 20px;', &
'        }', &
'    </style>', &
'</head>', &
'<body>', &
bt, &
'  <pre class="mermaid">'

call print_mermaid_graph(html_unit,package_name)

write(html_unit,'(*(A,/))') &
'  </pre>', &
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
'</body>', &
'</html>'

    end subroutine

end module
