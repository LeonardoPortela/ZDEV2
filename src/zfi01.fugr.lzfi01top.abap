FUNCTION-POOL ZFI01.                        "MESSAGE-ID ..


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

DATA: tg_texttable      TYPE y_text_table,
      wa_texttable type y_text,
      wa_thead type thead,
      tg_line type table of tline with header line,
      wg_line type tline.


*----------------------------------------------------------------------*
* Containers
*----------------------------------------------------------------------*
*Declarações utilizadas para utilização de containers.
DATA: obj_texto      TYPE REF TO cl_gui_textedit.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: gn_fname type thead-tdname,
      KKBER type KNKK-KKBER, "#EC CI_USAGE_OK[2227014]
      KUNNR TYPE KNKK-KUNNR, "#EC CI_USAGE_OK[2227014]
      gn_fid type  THEAD-TDID,
      gn_language type THEAD-TDSPRAS.
*----------------------------------------------------------------------*
* Objetos                                                              *
*----------------------------------------------------------------------*
DATA: container  TYPE REF TO  cl_gui_custom_container.
