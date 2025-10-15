FUNCTION-POOL zutil.                        "MESSAGE-ID ..

DATA: html_pagina TYPE string.

*-US192246-06.10.2025-#1922468-JT-inicio
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: it_fieldcat  TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      wa_fieldcat  TYPE ty_estrutura,
      ls_variant   TYPE disvariant,
      l_grid_title TYPE lvc_title.
*-US192246-06.10.2025-#1922468-JT-fim

* INCLUDE LZUTILD...                         " Local class definition
