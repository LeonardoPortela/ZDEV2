FUNCTION-POOL ZSD01                      MESSAGE-ID SV.


*----------------------------------------------------------------------*
* Tipo
*----------------------------------------------------------------------*
TYPES:
       BEGIN OF y_text,
          line(255),
       END OF y_text,
      y_text_table TYPE y_text OCCURS 0.

*----------------------------------------------------------------------*
* Tabela Interna
*----------------------------------------------------------------------*
* Reprovação
DATA: tg_texttable      TYPE y_text_table,
      wa_texttable type y_text,
      wa_thead type thead,
      tg_line type table of tline with header line,
      wg_line type tline.

* Aprovação
DATA: tg_texttable2      TYPE y_text_table,
      wa_texttable2 type y_text,
      wa_thead2 type thead,
      tg_line2 type table of tline with header line,
      wg_line2 type tline.


*----------------------------------------------------------------------*
* Containers
*----------------------------------------------------------------------*
*Declarações utilizadas para utilização de containers.
DATA: obj_texto      TYPE REF TO cl_gui_textedit,  "Reprovação
      obj_texto2      TYPE REF TO cl_gui_textedit. "Aprovação

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: gn_fname type thead-tdname,
      VBELN type VBAK-VBELN,
      gn_fid type  THEAD-TDID,
      gn_language type THEAD-TDSPRAS.
*----------------------------------------------------------------------*
* Objetos                                                              *
*----------------------------------------------------------------------*
DATA: container  TYPE REF TO  cl_gui_custom_container,  "Reprovação
      container2  TYPE REF TO  cl_gui_custom_container. "Aprovação
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZSD01T00                               . "view rel. data dcl.
