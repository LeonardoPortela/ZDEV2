FUNCTION ZDUE_SHOW_LOG_ERRO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_XML) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: T_ELEMENT_ARRAY     TYPE ZDE_ELEMENT_ARRAY_T,
        WL_RET_DUE_SISCOMEX TYPE ZST_RET_DUE_SISCOMEX,
        IT_XML_RET          TYPE TABLE OF ZXML.

  DATA : LCL_XML_DOC TYPE REF TO CL_XML_DOCUMENT,
         V_NODE      TYPE REF TO IF_IXML_NODE,
         V_ITERATOR  TYPE REF TO IF_IXML_NODE_ITERATOR,
         V_NODEMAP   TYPE REF TO IF_IXML_NAMED_NODE_MAP,
         V_ATTR      TYPE REF TO IF_IXML_NODE,
         V_COUNT     TYPE I,
         V_INDEX     TYPE I,
         V_NAME      TYPE STRING,
         V_PREFIX    TYPE STRING,
         V_VALUE     TYPE STRING,
         V_TAG       TYPE STRING,
         V_SUB_TAG   TYPE STRING,
         V_SUB       TYPE CHAR1,
         V_CHAR      TYPE CHAR2,
         V_SUBRC     TYPE SYSUBRC,
         VL_PARENT   TYPE STRING,
         VL_DATA_FMT TYPE STRING,
         VL_HORA_FMT TYPE STRING,
         "VL_CHAVE    TYPE STRING,
         VL_ITEM     TYPE I.

  CHECK I_XML IS NOT INITIAL.

  CLEAR: IT_SAIDA_1000[], IT_XML_RET[].

  APPEND I_XML TO IT_XML_RET.

  CREATE OBJECT LCL_XML_DOC.

  CALL METHOD LCL_XML_DOC->PARSE_TABLE
    EXPORTING
      TABLE   = IT_XML_RET
    RECEIVING
      RETCODE = DATA(_SUBRC).

  " a linha abaixo foi comentada para exibir as mensagens de erro do xml
  "CHECK _SUBRC = 0.

  V_NODE = LCL_XML_DOC->M_DOCUMENT.

  CHECK V_NODE IS NOT INITIAL.

  V_ITERATOR = V_NODE->CREATE_ITERATOR( ).
  V_NODE     = V_ITERATOR->GET_NEXT( ).

  WHILE V_NODE IS NOT INITIAL.

    CASE V_NODE->GET_TYPE( ).

      WHEN IF_IXML_NODE=>CO_NODE_ELEMENT.

        V_NAME    = V_NODE->GET_NAME( ).
        V_NODEMAP = V_NODE->GET_ATTRIBUTES( ).

        CASE V_NAME.  "Determinação de Parents para leitura
          WHEN: 'error'.
            VL_PARENT = V_NAME.
        ENDCASE.

        IF V_NODEMAP IS NOT INITIAL.

          V_VALUE = V_NODE->GET_VALUE( ).
          "Leitura de Childs
          CASE VL_PARENT.
            WHEN 'error'.
              CASE V_NAME.
                WHEN 'message'.
                  CLEAR: WA_SAIDA_1000.
                  WA_SAIDA_1000-MESSAGE    = V_VALUE.
                  WA_SAIDA_1000-MESSAGE_V1 = WA_SAIDA_1000-MESSAGE+000(125).
                  WA_SAIDA_1000-MESSAGE_V2 = WA_SAIDA_1000-MESSAGE+125(125).
                WHEN 'code'.
                  WA_SAIDA_1000-code  = V_VALUE.
                WHEN 'tag'.
                  WA_SAIDA_1000-tag   = V_VALUE.
                  APPEND WA_SAIDA_1000 TO IT_SAIDA_1000.
              ENDCASE.
          ENDCASE.

        ENDIF. "IF V_NODEMAP IS NOT INITIAL.

     ENDCASE.

     V_NODE = V_ITERATOR->GET_NEXT( ).

   ENDWHILE.

   CHECK IT_SAIDA_1000[] IS NOT INITIAL.

   CALL SCREEN 1000 STARTING AT 02 02.


ENDFUNCTION.
