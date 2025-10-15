"Name: \PR:SAPMF02C\FO:SCHLUSS_BEARBEITUNG\SE:BEGIN\EI
ENHANCEMENT 0 Z_FI_WF_ADM_LIMITE_CREDITO.

TYPES : BEGIN OF y_itab,
          kunnr TYPE knkk-kunnr,              "#EC CI_USAGE_OK[2227014]
          kkber TYPE knkk-kkber,              "#EC CI_USAGE_OK[2227014]
        END OF y_itab.

DATA : l_itab       TYPE y_itab.

DATA: v_object_key TYPE sweinstcou-objkey,
      t_cont       TYPE TABLE OF swr_cont,
      w_cont       TYPE swr_cont.

DATA: t_worklist  TYPE TABLE OF swr_wihdr,
      w_worklist  TYPE swr_wihdr,
      t_container TYPE TABLE OF swr_cont,
      w_container TYPE swr_cont.

CONSTANTS: c_bo TYPE swo_objtyp VALUE 'BUS1010'.


* Inciar WF de Administração de Limite de Crédito se o valor do
* limite for alterado.
IF yknkk-klimk NE knkk-klimk.                 "#EC CI_USAGE_OK[2227014]

*   Montagem da chave -> Cliente+Área de Crédito
  l_itab-kunnr = knkk-kunnr.                  "#EC CI_USAGE_OK[2227014]
  l_itab-kkber = knkk-kkber.                  "#EC CI_USAGE_OK[2227014]
  v_object_key = l_itab.

* Caso já exista um WF com a chave cliente + área de Crédito
* esta parte do código irá excluir o WF antigo.

*   Função que busca os workflows existentes
  CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
    EXPORTING
      objtype               = 'BUS1010'
      objkey                = v_object_key
      top_level_items       = 'X'
      text                  = 'X'
      output_only_top_level = ' '
      language              = sy-langu
    TABLES
      worklist              = t_worklist.

  IF NOT t_worklist IS INITIAL.

    LOOP AT t_worklist INTO w_worklist.
*
**       Função que lê valores do container para verificar se o workflow
**       já foi aprovado, rejeitado.
*        CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
*          EXPORTING
*            workitem_id                    = w_worklist-wi_id
*          TABLES
*            SIMPLE_CONTAINER               = t_container.
*
*        IF NOT t_container IS INITIAL.
*
*          READ TABLE t_container INTO w_container WITH KEY
*                    element = 'REJECTED'.
*          IF sy-subrc = 0.
*            IF w_container-value EQ space.
*
*              READ TABLE t_container INTO w_container WITH KEY
*                        element = 'RELEASED'.
*              IF sy-subrc = 0.
*
*               Função que elimina o workflow.
      CALL FUNCTION 'SAP_WAPI_WORKITEM_DELETE'
        EXPORTING
          workitem_id       = w_worklist-wi_id
*         ACTUAL_AGENT      = SY-UNAME
          language          = sy-langu
          check_final_state = ' '
*         DO_COMMIT         = 'X'
*         INVOKE_EXIT       = 'X'
*                 IMPORTING
*         RETURN_CODE       =
*                 TABLES
*         MESSAGE_LINES     =
*         MESSAGE_STRUCT    =
        .

      CLEAR: t_container, w_container.
      REFRESH: t_container.

*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
    ENDLOOP.
  ENDIF.


  w_cont-element = 'KLIMK_NEW'.
  w_cont-value = knkk-klimk.                  "#EC CI_USAGE_OK[2227014]
  APPEND w_cont TO t_cont.


  CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
    EXPORTING
      object_type     = c_bo
      object_key      = v_object_key
      event           = 'Z_INICIARWF'
      commit_work     = 'X'
      event_language  = sy-langu
      language        = sy-langu
      user            = sy-uname
*     IFS_XML_CONTAINER       =
*     IMPORTING
*     RETURN_CODE     =
*     EVENT_ID        =
    TABLES
      input_container = t_cont.

  knkk-crblb = 'X'.                           "#EC CI_USAGE_OK[2227014]
ENDIF.


ENDENHANCEMENT.
