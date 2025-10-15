*&---------------------------------------------------------------------*
*&  Include           ZMMR126_TREE_VIEW
*&---------------------------------------------------------------------*

CONSTANTS:
  BEGIN OF C_TREE,
    COLUMN1 TYPE TV_ITMNAME VALUE 'Documentos',
    COLUMN2 TYPE TV_ITMNAME VALUE 'Imprimir',
    COLUMN3 TYPE TV_ITMNAME VALUE 'Descricao',
  END OF C_TREE.

TYPES: BEGIN OF TY_NODE_INFO.
TYPES: NODE_KEY      TYPE  TV_NODEKEY,
       ITEM_NAME     TYPE  TV_ITMNAME,
       TP_MOVIMENTO  TYPE ZTP_MOV,
       CH_REFERENCIA TYPE ZCH_REF,
       TIPO          TYPE CHAR03,
       DOC_PART_1    TYPE CHAR30,
       DOC_PART_2    TYPE CHAR30,
       DOC_PART_3    TYPE CHAR30,
       DOC_REF       TYPE CHAR30,
       AV_VBELN      TYPE VBELN_VL.
TYPES: END OF TY_NODE_INFO.

DATA: TREE         TYPE REF TO CL_GUI_COLUMN_TREE,
      NODE_TABLE   TYPE TREEV_NTAB,
      ITEM_TABLE   TYPE STANDARD TABLE OF MTREEITM,
      IT_TREE_INFO TYPE TABLE OF TY_NODE_INFO WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ATUALIZA_TREE USING P_ID_CARGA TYPE ZDE_ID_CARGA.

  DATA: OBJ_PEDAFIO_REPOM TYPE REF TO ZCL_REPOM_VIAGEM_VPR,
        WA_REGISTRO       TYPE ZLEST0123,
        NODE              TYPE TREEV_NODE,
        NODE_AV           TYPE TREEV_NODE,
        ITEM              TYPE MTREEITM,
        QTD_ITENS         TYPE I,
        IT_NODE           TYPE TREEV_NKS,
        ST_CIOT           TYPE RANGE OF ZST_CIOT,
        LC_TEXTO          TYPE STRING.

  CHECK TREE IS NOT INITIAL.

  TREE->DELETE_ALL_NODES( ).

  CLEAR: NODE_TABLE[], ITEM_TABLE[], IT_TREE_INFO[], IT_TREE_INFO.

  TRY .

      DATA(OBJ_CARGA) =
      ZCL_FACTORY_CARGA=>ZIF_FACTORY_CARGA~GET_INSTANCE(
        )->SET_FACTORY_OBJETO_ID( I_ID_CARGA = P_ID_CARGA
        )->GET_FACTORY_OBJETO(
        )->SET_REGISTRO( EXPORTING I_ID_CARGA = P_ID_CARGA  I_NO_ENQUEUE = ABAP_TRUE
        ).

      OBJ_CARGA->GET_ROMANEIO_ENTRADA( EXPORTING I_ID_CARGA  = P_ID_CARGA IMPORTING E_ROMANEIOS = DATA(ROMANEIOS) ).
      OBJ_CARGA->GET_INFO_ALV_APRESENTACAO( IMPORTING E_APRESENTACAO = DATA(LC_CARGA) ).

      QTD_ITENS = 0.

*      IF LC_CARGA-CARGA-CK_FRETE_ENTRADA EQ ABAP_TRUE.
*
*        LOOP AT LC_CARGA-NOTAS INTO DATA(WA_NOTA_ENTRADA).
*
*          ADD 1 TO QTD_ITENS.
*          NODE-NODE_KEY   = QTD_ITENS.
*          CONDENSE NODE-NODE_KEY NO-GAPS.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              INPUT  = NODE-NODE_KEY
*            IMPORTING
*              OUTPUT = NODE-NODE_KEY.
*
*          NODE-HIDDEN     = ' '. " The node is visible,
*          NODE-DISABLED   = ' '. " selectable,
*          NODE-ISFOLDER   = 'X'. " a folder.
*          NODE-EXPANDER   = ABAP_TRUE.
*
*          CLEAR ITEM.
*          ITEM-NODE_KEY  = NODE-NODE_KEY.
*          ITEM-ITEM_NAME = C_TREE-COLUMN1.
*          ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_FONT_DEFAULT. " Text Item
*          ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
*          ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
*          ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
*          CONCATENATE WA_NOTA_ENTRADA-ID_FORNECEDOR WA_NOTA_ENTRADA-NR_NOTA INTO ITEM-TEXT SEPARATED BY SPACE.
*          CONCATENATE ITEM-TEXT '-' WA_NOTA_ENTRADA-NM_SERIE INTO ITEM-TEXT.
*          CONCATENATE 'Frete da Entrada' ITEM-TEXT INTO ITEM-TEXT SEPARATED BY SPACE.
*          APPEND NODE-NODE_KEY TO IT_NODE.
*
*          DATA(LC_AUTORIZADO) = ABAP_TRUE.
*          IF WA_NOTA_ENTRADA-FTE_DOCNUM IS NOT INITIAL.
*            TRY .
*                ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = WA_NOTA_ENTRADA-FTE_DOCNUM
*                  )->SET_REGISTRO(
*                  EXPORTING
*                    I_DOCNUM           = WA_NOTA_ENTRADA-FTE_DOCNUM    " Nº documento
*                    I_SEM_BLOQUEIO     = ABAP_TRUE
*                  )->GET_CK_AUTORIZADO_USO(
*                  ).
*              CATCH ZCX_DOC_ELETRONICO.    " .
*                LC_AUTORIZADO = ABAP_FALSE.
*            ENDTRY.
*          ENDIF.
*
*          IF WA_NOTA_ENTRADA-FTE_DOCNUM IS INITIAL OR LC_AUTORIZADO EQ ABAP_FALSE.
*            NODE-N_IMAGE    = ICON_IMPORT_TRANSPORT_REQUEST.
*            NODE-EXP_IMAGE  = ICON_IMPORT_TRANSPORT_REQUEST.
*          ELSE.
*            NODE-N_IMAGE    = ICON_IMPORT_ALL_REQUESTS.
*            NODE-EXP_IMAGE  = ICON_IMPORT_ALL_REQUESTS.
*          ENDIF.
*          APPEND NODE TO NODE_TABLE.
*          APPEND ITEM TO ITEM_TABLE.
*
*          IF WA_NOTA_ENTRADA-PO_NUMBER IS INITIAL.
*            CONTINUE.
*          ENDIF.
*
*          "'FPO -> "Pedido de Compra
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-PO_NUMBER WA_NOTA_ENTRADA-PO_ITEM  '' 'FPO' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          IF WA_NOTA_ENTRADA-AV_VBELN IS INITIAL.
*            CONTINUE.
*          ENDIF.
*
*          "'FAV -> "Avido de Recebimento
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-AV_VBELN  '' '' 'FAV' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          IF WA_NOTA_ENTRADA-FTE_TKNUM IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-FTE_TKNUM   ''    '' 'FDT' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_ID_PROC_CLIENTE_VT
*            EXPORTING
*              I_TKNUM           = WA_NOTA_ENTRADA-FTE_TKNUM
*            RECEIVING
*              E_ID_PROC_CLIENTE = DATA(LC_ID_PROC_CLIENTE)
*            EXCEPTIONS
*              NAO_ENCONTRADO    = 1
*              OTHERS            = 2.
*
*          IF SY-SUBRC IS INITIAL.
*            CREATE OBJECT OBJ_PEDAFIO_REPOM
*              EXPORTING
*                I_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE.
*
*            OBJ_PEDAFIO_REPOM->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_REGISTRO ).
*            CLEAR: OBJ_PEDAFIO_REPOM.
*            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_REGISTRO-ID_VIAGEM_CODIGO  '' WA_REGISTRO-ID_PROC_CLIENTE 'FP1' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*          ENDIF.
*
*          IF WA_NOTA_ENTRADA-FTE_FKNUM IS INITIAL.
*            CONTINUE.
*          ENDIF.
*
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-FTE_FKNUM  ''    '' 'FDC' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          IF WA_NOTA_ENTRADA-FTE_VBELN_VA IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-FTE_VBELN_VA  ''    '' 'FOT' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          IF WA_NOTA_ENTRADA-FTE_VBELN_VF IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-FTE_VBELN_VF ''    '' 'FFT' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          IF WA_NOTA_ENTRADA-FTE_DOCNUM IS INITIAL.
*            CONTINUE.
*          ENDIF.
*          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA_ENTRADA-FTE_DOCNUM ''    '' 'FFF' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*
*          ST_CIOT = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '5' HIGH = '5' ) ( LOW = '6' HIGH = '6' ) ).
*          SELECT SINGLE * INTO @DATA(WA_CONTRATO)
*            FROM ZCTE_CIOT
*           WHERE DOCNUM  EQ @WA_NOTA_ENTRADA-FTE_DOCNUM
*             AND ST_CIOT IN @ST_CIOT.
*
*          IF SY-SUBRC IS INITIAL.
*            "'SCV -> "Contrato de Viagem
*            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_CONTRATO-NUCONTRATO '' WA_CONTRATO-DOCNUM 'FCV' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*          ENDIF.
*
*          SELECT SINGLE * INTO @DATA(WA_MDFE)
*            FROM ZSDT0105 AS VC
*           WHERE VC~DOCNUM EQ @WA_NOTA_ENTRADA-FTE_DOCNUM
*             AND EXISTS ( SELECT * FROM ZSDT0102 AS MD WHERE MD~NMDFE EQ VC~NMDFE AND STATUS EQ '1' ).
*
*          IF SY-SUBRC IS INITIAL.
*            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_MDFE-DOCNUM_REF '' WA_MDFE-NMDFE 'FMD' NODE ITEM '' WA_NOTA_ENTRADA-AV_VBELN CHANGING QTD_ITENS.
*          ENDIF.
*
*        ENDLOOP.
*
*      ENDIF.

      LOOP AT LC_CARGA-NOTAS INTO DATA(WA_NOTA).

*        ADD 1 TO qtd_itens.
*        node_nf-node_key   = qtd_itens.
*        CONDENSE node_nf-node_key NO-GAPS.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = node_nf-node_key
*          IMPORTING
*            output = node_nf-node_key.
*
*        node_nf-hidden     = ' '. " The node is visible,
*        node_nf-disabled   = ' '. " selectable,
*        node_nf-isfolder   = 'X'. " a folder.
*        node_nf-expander   = abap_true.
*        node_nf-n_image    = icon_incoming_task.
*        node_nf-exp_image  = icon_incoming_task.
*
*        CLEAR item.
*        item-node_key  = node_nf-node_key.
*        item-item_name = c_tree-column1.
*        item-class     = cl_gui_list_tree=>item_font_default. " Text Item
*        item-alignment = cl_gui_list_tree=>align_auto.
*        item-style     = cl_gui_list_tree=>style_intensified.
*        item-font      = cl_gui_list_tree=>item_font_prop.
*        item-text      = |Nota Fiscal { zcl_str=>trim( CONV #( wa_nota-nr_nota ) )->get( ) }-{ zcl_str=>trim( CONV #( wa_nota-nm_serie ) )->get( ) }|.
*        APPEND node_nf-node_key TO it_node.
*
*        APPEND node_nf TO node_table.
*        APPEND item TO item_table.

        "Tem frete

        IF ROMANEIOS[] IS INITIAL AND WA_NOTA-AV_VBELN IS NOT INITIAL.


          ADD 1 TO QTD_ITENS.
          NODE-NODE_KEY   = QTD_ITENS.
          CONDENSE NODE-NODE_KEY NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = NODE-NODE_KEY
            IMPORTING
              OUTPUT = NODE-NODE_KEY.

          NODE-HIDDEN     = ' '. " The node is visible,
          NODE-DISABLED   = ' '. " selectable,
          NODE-ISFOLDER   = 'X'. " a folder.
          NODE-EXPANDER   = ABAP_TRUE.

          CLEAR ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN1.
          ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_FONT_DEFAULT. " Text Item
          ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
          ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
          ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
          CONCATENATE WA_NOTA-ID_FORNECEDOR WA_NOTA-NR_NOTA INTO ITEM-TEXT SEPARATED BY SPACE.
          CONCATENATE ITEM-TEXT '-' WA_NOTA-NM_SERIE INTO ITEM-TEXT.
          CONCATENATE 'Frete da Entrada' ITEM-TEXT INTO ITEM-TEXT SEPARATED BY SPACE.
          APPEND NODE-NODE_KEY TO IT_NODE.

          DATA(LC_AUTORIZADO) = ABAP_TRUE.
          IF WA_NOTA-FTE_DOCNUM IS NOT INITIAL.
            TRY .
                ZCL_DOC_ELETRONICO=>ZIF_DOC_ELETRONICO~GET_INSTANCE( I_DOCNUM = WA_NOTA-FTE_DOCNUM
                  )->SET_REGISTRO(
                  EXPORTING
                    I_DOCNUM           = WA_NOTA-FTE_DOCNUM    " Nº documento
                    I_SEM_BLOQUEIO     = ABAP_TRUE
                  )->GET_CK_AUTORIZADO_USO(
                  ).
              CATCH ZCX_DOC_ELETRONICO.    " .
                LC_AUTORIZADO = ABAP_FALSE.
            ENDTRY.
          ENDIF.

          IF WA_NOTA-FTE_DOCNUM IS INITIAL OR LC_AUTORIZADO EQ ABAP_FALSE.
            NODE-N_IMAGE    = ICON_IMPORT_TRANSPORT_REQUEST.
            NODE-EXP_IMAGE  = ICON_IMPORT_TRANSPORT_REQUEST.
          ELSE.
            NODE-N_IMAGE    = ICON_IMPORT_ALL_REQUESTS.
            NODE-EXP_IMAGE  = ICON_IMPORT_ALL_REQUESTS.
          ENDIF.
          APPEND NODE TO NODE_TABLE.
          APPEND ITEM TO ITEM_TABLE.

          IF WA_NOTA-PO_NUMBER IS INITIAL.
            CONTINUE.
          ENDIF.

          "'FPO -> "Pedido de Compra
          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-PO_NUMBER WA_NOTA-PO_ITEM  '' 'FPO' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

          IF WA_NOTA-AV_VBELN IS INITIAL.
            CONTINUE.
          ENDIF.

          "'FAV -> "Avido de Recebimento
          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-AV_VBELN  '' '' 'FAV' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

          IF WA_NOTA-FTE_TKNUM IS INITIAL.
            CONTINUE.
          ENDIF.

          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_TKNUM   ''    '' 'FDT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

          CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_ID_PROC_CLIENTE_VT
            EXPORTING
              I_TKNUM           = WA_NOTA-FTE_TKNUM
            RECEIVING
              E_ID_PROC_CLIENTE = DATA(LC_ID_PROC_CLIENTE)
            EXCEPTIONS
              NAO_ENCONTRADO    = 1
              OTHERS            = 2.

          IF SY-SUBRC IS INITIAL.
            CREATE OBJECT OBJ_PEDAFIO_REPOM
              EXPORTING
                I_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE.

            OBJ_PEDAFIO_REPOM->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_REGISTRO ).
            CLEAR: OBJ_PEDAFIO_REPOM.
            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_REGISTRO-ID_VIAGEM_CODIGO  '' WA_REGISTRO-ID_PROC_CLIENTE 'FP1' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
          ENDIF.

          IF WA_NOTA-FTE_FKNUM IS INITIAL.
            CONTINUE.
          ENDIF.

          PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_FKNUM  ''    '' 'FDC' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

          IF WA_NOTA-FTE_VBELN_VA IS NOT INITIAL.
            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_VBELN_VA  ''    '' 'FOT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
          ENDIF.

          IF WA_NOTA-FTE_VBELN_VF IS NOT INITIAL.
            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_VBELN_VF ''    '' 'FFT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
          ENDIF.

          IF WA_NOTA-FTE_DOCNUM IS NOT INITIAL.

            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_DOCNUM ''    '' 'FFF' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

            ST_CIOT = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '5' HIGH = '5' ) ( LOW = '6' HIGH = '6' ) ).
            SELECT SINGLE * INTO @DATA(WA_CONTRATO)
              FROM ZCTE_CIOT
             WHERE DOCNUM  EQ @WA_NOTA-FTE_DOCNUM
               AND ST_CIOT IN @ST_CIOT.

            IF SY-SUBRC IS INITIAL.
              "'SCV -> "Contrato de Viagem
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_CONTRATO-NUCONTRATO '' WA_CONTRATO-DOCNUM 'FCV' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

            SELECT SINGLE VC~DOCNUM_REF, VC~DOCNUM, VC~NMDFE INTO @DATA(WA_MDFE)
              FROM ZSDT0105 AS VC
             INNER JOIN ZSDT0102 AS MF ON MF~DOCNUM EQ VC~DOCNUM_REF AND MF~ESTORNADO EQ @ABAP_FALSE
             WHERE VC~DOCNUM EQ @WA_NOTA-FTE_DOCNUM
               AND EXISTS ( SELECT * FROM ZSDT0102 AS MD WHERE MD~NMDFE EQ VC~NMDFE AND STATUS EQ '1' ).

            IF SY-SUBRC IS INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_MDFE-DOCNUM_REF '' WA_MDFE-NMDFE 'FMD' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

          ELSEIF WA_NOTA-FTE_DOCNUM_MDFE IS NOT INITIAL.

            SELECT SINGLE MF~DOCNUM, MF~NMDFE INTO @DATA(WA_MDFE_3)
              FROM ZSDT0102 AS MF
             WHERE MF~DOCNUM    EQ @WA_NOTA-FTE_DOCNUM_MDFE
               AND MF~ESTORNADO EQ @ABAP_FALSE.

            IF SY-SUBRC IS INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_MDFE_3-DOCNUM '' WA_MDFE_3-NMDFE 'FMD' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

          ENDIF.
        ENDIF.

        LOOP AT ROMANEIOS INTO DATA(WA_ROMANEIO) WHERE ID_NOTA EQ WA_NOTA-ID_NOTA.

          CLEAR: NODE.

          ADD 1 TO QTD_ITENS.
          NODE-NODE_KEY   = QTD_ITENS.
          CONDENSE NODE-NODE_KEY NO-GAPS.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = NODE-NODE_KEY
            IMPORTING
              OUTPUT = NODE-NODE_KEY.

          NODE-HIDDEN     = ' '. " The node is visible,
          NODE-DISABLED   = ' '. " selectable,
          NODE-ISFOLDER   = 'X'. " a folder.
          NODE-EXPANDER   = ABAP_TRUE.
          "node-relatkey   = node_nf-node_key.
          "node-relatship  = cl_gui_column_tree=>relat_last_child.

          CLEAR ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN1.
          ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_FONT_DEFAULT. " Text Item
          ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
          ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
          ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
          CONCATENATE 'Romaneio Entrada' WA_ROMANEIO-NR_ROMANEIO INTO ITEM-TEXT SEPARATED BY SPACE.
          APPEND NODE-NODE_KEY TO IT_NODE.

*          READ TABLE LC_CARGA-NOTAS WITH KEY CH_REFERENCIA_ENT = WA_ROMANEIO-CH_REFERENCIA INTO DATA(WA_NOTA).
*          IF SY-SUBRC IS INITIAL.

          IF WA_NOTA-DOCNUM IS INITIAL.
            NODE-N_IMAGE    = ICON_WF_WORKITEM_WAITING.
            NODE-EXP_IMAGE  = ICON_WF_WORKITEM_WAITING.
          ELSE.
            NODE-N_IMAGE    = ICON_WF_WORKITEM_COMPLETED.
            NODE-EXP_IMAGE  = ICON_WF_WORKITEM_COMPLETED.
          ENDIF.
          APPEND NODE TO NODE_TABLE.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN2.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'PDF'.
          ITEM-T_IMAGE   = ICON_PDF.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'E'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
          IT_TREE_INFO-TIPO          = 'ERO'.
          APPEND IT_TREE_INFO.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN3.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'Romaneio'.
          ITEM-T_IMAGE   = ICON_OUTPUT_REQUEST.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'E'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN3.
          IT_TREE_INFO-TIPO          = 'ETK'.
          APPEND IT_TREE_INFO.

          "'EPO -> "Pedido de Compra
          PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-PO_NUMBER WA_NOTA-PO_ITEM  '' 'EPO' NODE ITEM '' '' CHANGING QTD_ITENS.

          IF WA_NOTA-AV_VBELN IS NOT INITIAL.

*            ADD 1 TO qtd_itens.
*            node-node_key   = qtd_itens.
*            CONDENSE node-node_key NO-GAPS.
*
*            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*              EXPORTING
*                input  = node-node_key
*              IMPORTING
*                output = node-node_key.
*
*            node_av-hidden     = ' '. " The node is visible,
*            node_av-disabled   = ' '. " selectable,
*            node_av-isfolder   = 'X'. " a folder.
*            node_av-expander   = abap_true.
*            node_av-relatkey   = node-node_key.
*            node_av-relatship  = cl_gui_column_tree=>relat_last_child.
*
*            CLEAR item.
*            item-node_key  = node_av-node_key.
*            item-item_name = c_tree-column1.
*            item-class     = cl_gui_list_tree=>item_font_default. " Text Item
*            item-alignment = cl_gui_list_tree=>align_auto.
*            item-style     = cl_gui_list_tree=>style_intensified.
*            item-font      = cl_gui_list_tree=>item_font_prop.
*            item-text      = |Frete da Entrada|.
**          CONCATENATE WA_NOTA-ID_FORNECEDOR WA_NOTA_ENTRADA-NR_NOTA INTO ITEM-TEXT SEPARATED BY SPACE.
**          CONCATENATE ITEM-TEXT '-' WA_NOTA_ENTRADA-NM_SERIE INTO ITEM-TEXT.
**          CONCATENATE 'Frete da Entrada' ITEM-TEXT INTO ITEM-TEXT SEPARATED BY SPACE.
*            APPEND node-node_key TO it_node.
*
*            DATA(lc_autorizado) = abap_true.
*            IF wa_nota-fte_docnum IS NOT INITIAL.
*              TRY .
*                  zcl_doc_eletronico=>zif_doc_eletronico~get_instance( i_docnum = wa_nota-fte_docnum
*                    )->set_registro(
*                    EXPORTING
*                      i_docnum           = wa_nota-fte_docnum    " Nº documento
*                      i_sem_bloqueio     = abap_true
*                    )->get_ck_autorizado_uso(
*                    ).
*                CATCH zcx_doc_eletronico.    " .
*                  lc_autorizado = abap_false.
*              ENDTRY.
*            ENDIF.
*            IF wa_nota-fte_docnum IS INITIAL OR lc_autorizado EQ abap_false.
*              node-n_image    = icon_import_transport_request.
*              node-exp_image  = icon_import_transport_request.
*            ELSE.
*              node-n_image    = icon_import_all_requests.
*              node-exp_image  = icon_import_all_requests.
*            ENDIF.
*            APPEND node TO node_table.
*            APPEND item TO item_table.

            "'FAV -> "Avido de Recebimento
            PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-AV_VBELN  '' '' 'FAV' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

            IF WA_NOTA-AV_VBELN IS NOT INITIAL AND WA_NOTA-FTE_TKNUM IS INITIAL.
              SELECT SINGLE * INTO @DATA(WA_ZLEST0108)
                FROM ZLEST0108
               WHERE VBELN EQ @WA_NOTA-AV_VBELN.

              IF SY-SUBRC IS INITIAL.
                WA_NOTA-FTE_TKNUM    = WA_ZLEST0108-DOC_TRANSP.
                WA_NOTA-FTE_FKNUM    = WA_ZLEST0108-FKNUM.
                WA_NOTA-FTE_VBELN_VA = WA_ZLEST0108-OV_FRETE.
                WA_NOTA-FTE_VBELN_VF = WA_ZLEST0108-FATURA_FRETE.
                WA_NOTA-FTE_DOCNUM   = WA_ZLEST0108-NRO_NF_FRETE.
              ENDIF.
            ENDIF.

            IF WA_NOTA-FTE_TKNUM IS NOT INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_TKNUM   ''    '' 'FDT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

              CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_ID_PROC_CLIENTE_VT
                EXPORTING
                  I_TKNUM           = WA_NOTA-FTE_TKNUM
                RECEIVING
                  E_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE
                EXCEPTIONS
                  NAO_ENCONTRADO    = 1
                  OTHERS            = 2.

              IF SY-SUBRC IS INITIAL.
                CREATE OBJECT OBJ_PEDAFIO_REPOM
                  EXPORTING
                    I_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE.

                OBJ_PEDAFIO_REPOM->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_REGISTRO ).
                CLEAR: OBJ_PEDAFIO_REPOM.
                PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_REGISTRO-ID_VIAGEM_CODIGO  '' WA_REGISTRO-ID_PROC_CLIENTE 'FP1' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
              ENDIF.

            ENDIF.

            IF WA_NOTA-FTE_FKNUM IS NOT INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_FKNUM  ''    '' 'FDC' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

            IF WA_NOTA-FTE_VBELN_VA IS NOT INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_VBELN_VA  ''    '' 'FOT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

            IF WA_NOTA-FTE_VBELN_VF IS NOT INITIAL.
              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_VBELN_VF ''    '' 'FFT' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
            ENDIF.

            IF WA_NOTA-FTE_DOCNUM IS NOT INITIAL.

              PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_NOTA-FTE_DOCNUM ''    '' 'FFF' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.

              ST_CIOT = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '5' HIGH = '5' ) ( LOW = '6' HIGH = '6' ) ).
              SELECT SINGLE * INTO @WA_CONTRATO
                FROM ZCTE_CIOT
               WHERE DOCNUM  EQ @WA_NOTA-FTE_DOCNUM
                 AND ST_CIOT IN @ST_CIOT.

              IF SY-SUBRC IS INITIAL.
                "'SCV -> "Contrato de Viagem
                PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_CONTRATO-NUCONTRATO '' WA_CONTRATO-DOCNUM 'FCV' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
              ENDIF.

              SELECT SINGLE VC~DOCNUM_REF, VC~NMDFE INTO @DATA(WA_MDFE_2)
                FROM ZSDT0105 AS VC
               WHERE VC~DOCNUM EQ @WA_NOTA-FTE_DOCNUM
                 AND EXISTS ( SELECT * FROM ZSDT0102 AS MD WHERE MD~NMDFE EQ VC~NMDFE AND STATUS EQ '1' AND MD~ESTORNADO EQ @ABAP_FALSE ).

              IF SY-SUBRC IS INITIAL.
                PERFORM ADD_DOCUMENTO_TREE USING 'F' '' WA_MDFE_2-DOCNUM_REF '' WA_MDFE_2-NMDFE 'FMD' NODE ITEM '' WA_NOTA-AV_VBELN CHANGING QTD_ITENS.
              ENDIF.

            ENDIF.

          ENDIF.

          ""'EAV -> "Avido de Recebimento
          "PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-AV_VBELN  ''               '' 'EAV' NODE ITEM '' '' CHANGING QTD_ITENS.
          "'EDM -> "Documento de Material
          PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-MM_MBLNR  WA_NOTA-MM_MJAHR '' 'EDM' NODE ITEM '' '' CHANGING QTD_ITENS.
          "'EFT -> "Documento de Faturamento
          PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-FT_BELNR  WA_NOTA-FT_GJAHR '' 'EFT' NODE ITEM '' '' CHANGING QTD_ITENS.
          "'EFS -> "Documento Fiscal
          PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-DOCNUM    ''               '' 'EFS' NODE ITEM '' '' CHANGING QTD_ITENS.
          "'EMS -> "Documento de Material - Sobra
          PERFORM ADD_DOCUMENTO_TREE USING 'E' WA_ROMANEIO-CH_REFERENCIA WA_NOTA-MM_MBLNR_SOBRA  WA_NOTA-MM_MJAHR_SOBRA '' 'EMS' NODE ITEM '' '' CHANGING QTD_ITENS.

*          ELSE.
*            NODE-N_IMAGE    = ICON_WF_WORKITEM_READY.
*            NODE-EXP_IMAGE  = ICON_WF_WORKITEM_READY.
*            APPEND NODE TO NODE_TABLE.
*            APPEND ITEM TO ITEM_TABLE.
*          ENDIF.
        ENDLOOP.

      ENDLOOP.


      " SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA
      " SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA SAÍDA
      OBJ_CARGA->GET_ROMANEIO_SAIDA( EXPORTING I_ID_CARGA  = P_ID_CARGA IMPORTING E_ROMANEIOS = ROMANEIOS ).

      LOOP AT ROMANEIOS INTO WA_ROMANEIO.

        CLEAR: NODE.
        ADD 1 TO QTD_ITENS.
        NODE-NODE_KEY   = QTD_ITENS.
        CONDENSE NODE-NODE_KEY NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = NODE-NODE_KEY
          IMPORTING
            OUTPUT = NODE-NODE_KEY.

        NODE-HIDDEN     = ' '. " The node is visible,
        NODE-DISABLED   = ' '. " selectable,
        NODE-ISFOLDER   = 'X'. " a folder.
        NODE-EXPANDER   = ABAP_TRUE.

        CLEAR ITEM.
        ITEM-NODE_KEY  = NODE-NODE_KEY.
        ITEM-ITEM_NAME = C_TREE-COLUMN1.
        ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_FONT_DEFAULT. " Text Item
        ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
        ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
        ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
        CONCATENATE 'Romaneio Saída' WA_ROMANEIO-NR_ROMANEIO INTO ITEM-TEXT SEPARATED BY SPACE.

        APPEND NODE-NODE_KEY TO IT_NODE.

        IF WA_ROMANEIO-ST_PROC IS NOT INITIAL.

          IF WA_ROMANEIO-ST_PROC NE '99'.
            NODE-N_IMAGE    = ICON_WF_WORKITEM_WAITING.
            NODE-EXP_IMAGE  = ICON_WF_WORKITEM_WAITING.
          ELSE.
            NODE-N_IMAGE    = ICON_WF_WORKITEM_COMPLETED.
            NODE-EXP_IMAGE  = ICON_WF_WORKITEM_COMPLETED.
          ENDIF.

          APPEND NODE TO NODE_TABLE.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN2.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'PDF'.
          ITEM-T_IMAGE   = ICON_PDF.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'S'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
          IT_TREE_INFO-TIPO          = 'SRO'.
          APPEND IT_TREE_INFO.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN3.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'Romaneio'.
          ITEM-T_IMAGE   = ICON_OUTPUT_REQUEST.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'E'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN3.
          IT_TREE_INFO-TIPO          = 'STK'.
          APPEND IT_TREE_INFO.

          "'SDM -> "Documento de Material
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-DOC_MATERIAL_E WA_ROMANEIO-ANO_MATERIAL_E '' 'SDM' NODE ITEM '' '' CHANGING QTD_ITENS.

          CASE OBJ_CARGA->CARGA-IN_TRANSFERENCIA.
            WHEN ABAP_TRUE.
              "'SOV -> "Ordem de Venda
              PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-VBELN        ''    '' 'SPO' NODE ITEM '' '' CHANGING QTD_ITENS.
            WHEN ABAP_FALSE.
              "'SOV -> "Ordem de Venda
              PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-VBELN        ''    '' 'SOV' NODE ITEM '' '' CHANGING QTD_ITENS.
          ENDCASE.

          "'SRM -> "Remessa
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-DOC_REM      ''    '' 'SRM' NODE ITEM '' '' CHANGING QTD_ITENS.

          "'SVF -> "Fatura Da Mercadoria
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-FATURA_PROD
            IMPORTING
              OUTPUT = WA_ROMANEIO-FATURA_PROD.

          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-FATURA_PROD  ''    '' 'SVF' NODE ITEM '' '' CHANGING QTD_ITENS.

          "'SFS -> "Documento Fiscal Mercadoria
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-NRO_NF_PROD
            IMPORTING
              OUTPUT = WA_ROMANEIO-NRO_NF_PROD.

          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-NRO_NF_PROD  ''    '' 'SFS' NODE ITEM '' '' CHANGING QTD_ITENS.

          SELECT SINGLE VC~DOCNUM_REF, VC~NMDFE INTO @WA_MDFE_2
            FROM ZSDT0105 AS VC
           WHERE VC~DOCNUM EQ @WA_ROMANEIO-NRO_NF_PROD
             AND EXISTS ( SELECT * FROM ZSDT0102 AS MD WHERE MD~NMDFE EQ VC~NMDFE AND STATUS EQ '1' AND MD~ESTORNADO EQ @ABAP_FALSE ).

          IF SY-SUBRC IS INITIAL.
            "'SMD -> "MDFE
            PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_MDFE-DOCNUM_REF '' WA_MDFE-NMDFE 'SMD' NODE ITEM '' '' CHANGING QTD_ITENS.
          ENDIF.

          "'SDT -> "Documento de Transporte
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-DOC_TRANSP
            IMPORTING
              OUTPUT = WA_ROMANEIO-DOC_TRANSP.

          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-DOC_TRANSP   ''    '' 'SDT' NODE ITEM '' '' CHANGING QTD_ITENS.

          IF WA_ROMANEIO-DOC_TRANSP IS NOT INITIAL.

            CALL METHOD ZCL_REPOM_VIAGEM_VPR=>GET_ID_PROC_CLIENTE_VT
              EXPORTING
                I_TKNUM           = WA_ROMANEIO-DOC_TRANSP
              RECEIVING
                E_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE
              EXCEPTIONS
                NAO_ENCONTRADO    = 1
                OTHERS            = 2.

            IF SY-SUBRC IS INITIAL.
              CREATE OBJECT OBJ_PEDAFIO_REPOM
                EXPORTING
                  I_ID_PROC_CLIENTE = LC_ID_PROC_CLIENTE.

              OBJ_PEDAFIO_REPOM->ZIF_CADASTRO~GET_REGISTRO( IMPORTING E_REGISTRO = WA_REGISTRO ).
              CLEAR: OBJ_PEDAFIO_REPOM.
              PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_REGISTRO-ID_VIAGEM_CODIGO  '' WA_REGISTRO-ID_PROC_CLIENTE 'SP1' NODE ITEM '' '' CHANGING QTD_ITENS.
            ENDIF.

          ENDIF.

          "'SDC -> "Documento de Custo
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-FKNUM
            IMPORTING
              OUTPUT = WA_ROMANEIO-FKNUM.
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-FKNUM        ''    '' 'SDC' NODE ITEM '' '' CHANGING QTD_ITENS.

          "'SOT -> "Ordem de Venda Frete
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-OV_FRETE
            IMPORTING
              OUTPUT = WA_ROMANEIO-OV_FRETE.
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-OV_FRETE     ''    '' 'SOT' NODE ITEM '' '' CHANGING QTD_ITENS.

          "'SFT -> "Fatura do Frete
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-FATURA_FRETE
            IMPORTING
              OUTPUT = WA_ROMANEIO-FATURA_FRETE.
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-FATURA_FRETE ''    '' 'SFT' NODE ITEM '' '' CHANGING QTD_ITENS.

          "'SFF -> "Documento Fiscal Frete
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              INPUT  = WA_ROMANEIO-NRO_NF_FRETE
            IMPORTING
              OUTPUT = WA_ROMANEIO-NRO_NF_FRETE.
          PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_ROMANEIO-NRO_NF_FRETE ''    '' 'SFF' NODE ITEM '' '' CHANGING QTD_ITENS.

          IF WA_ROMANEIO-NRO_NF_FRETE IS NOT INITIAL.

            ST_CIOT = VALUE #( SIGN = 'I' OPTION = 'EQ' ( LOW = '5' HIGH = '5' ) ( LOW = '6' HIGH = '6' ) ).
            SELECT SINGLE * INTO @WA_CONTRATO
              FROM ZCTE_CIOT
             WHERE DOCNUM  EQ @WA_ROMANEIO-NRO_NF_FRETE
               AND ST_CIOT IN @ST_CIOT.

            IF SY-SUBRC IS INITIAL.
              "'SCV -> "Contrato de Viagem
              PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_CONTRATO-NUCONTRATO '' WA_CONTRATO-DOCNUM 'SCV' NODE ITEM '' '' CHANGING QTD_ITENS.
            ENDIF.

            SELECT SINGLE VC~DOCNUM_REF, VC~NMDFE INTO @WA_MDFE_2
              FROM ZSDT0105 AS VC
             WHERE VC~DOCNUM EQ @WA_ROMANEIO-NRO_NF_FRETE
               AND EXISTS ( SELECT * FROM ZSDT0102 AS MD WHERE MD~NMDFE EQ VC~NMDFE AND STATUS EQ '1' AND MD~ESTORNADO EQ @ABAP_FALSE ).

            IF SY-SUBRC IS INITIAL.
              "'SMD -> "MDFE
              PERFORM ADD_DOCUMENTO_TREE USING 'S' WA_ROMANEIO-CH_REFERENCIA WA_MDFE-DOCNUM_REF '' WA_MDFE-NMDFE 'SMD' NODE ITEM '' '' CHANGING QTD_ITENS.
            ENDIF.

          ENDIF.

        ELSE.
          NODE-N_IMAGE    = ICON_WF_WORKITEM_READY.
          NODE-EXP_IMAGE  = ICON_WF_WORKITEM_READY.
          APPEND NODE TO NODE_TABLE.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN2.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'PDF'.
          ITEM-T_IMAGE   = ICON_PDF.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'S'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
          IT_TREE_INFO-TIPO          = 'SRO'.
          APPEND IT_TREE_INFO.

          CLEAR: ITEM.
          ITEM-NODE_KEY  = NODE-NODE_KEY.
          ITEM-ITEM_NAME = C_TREE-COLUMN3.
          ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
          ITEM-TEXT      = 'Romaneio'.
          ITEM-T_IMAGE   = ICON_OUTPUT_REQUEST.
          APPEND ITEM TO ITEM_TABLE.

          CLEAR: IT_TREE_INFO.
          IT_TREE_INFO-TP_MOVIMENTO  = 'E'.
          IT_TREE_INFO-CH_REFERENCIA = WA_ROMANEIO-CH_REFERENCIA.
          IT_TREE_INFO-NODE_KEY      = NODE-NODE_KEY.
          IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN3.
          IT_TREE_INFO-TIPO          = 'STK'.
          APPEND IT_TREE_INFO.

        ENDIF.
      ENDLOOP.

      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DESCRIBE TABLE LC_CARGA-SOLICITACOES LINES DATA(LC_QTD_SOLICITACOES).

      IF LC_QTD_SOLICITACOES IS NOT INITIAL.
        ADD 1 TO QTD_ITENS.
        NODE-NODE_KEY   = QTD_ITENS.
        CONDENSE NODE-NODE_KEY NO-GAPS.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = NODE-NODE_KEY
          IMPORTING
            OUTPUT = NODE-NODE_KEY.

        NODE-HIDDEN     = ' '. " The node is visible,
        NODE-DISABLED   = ' '. " selectable,
        NODE-ISFOLDER   = 'X'. " a folder.
        NODE-EXPANDER   = ABAP_TRUE.

        CLEAR ITEM.
        ITEM-NODE_KEY  = NODE-NODE_KEY.
        ITEM-ITEM_NAME = C_TREE-COLUMN1.
        ITEM-CLASS     = CL_GUI_LIST_TREE=>ITEM_FONT_DEFAULT. " Text Item
        ITEM-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
        ITEM-STYLE     = CL_GUI_LIST_TREE=>STYLE_INTENSIFIED.
        ITEM-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
        ITEM-TEXT      = 'WorkFlow SoftExpert'.
        APPEND NODE-NODE_KEY TO IT_NODE.

        APPEND NODE TO NODE_TABLE.
        APPEND ITEM TO ITEM_TABLE.

      ENDIF.

      LOOP AT LC_CARGA-SOLICITACOES INTO DATA(WA_SOLICITACOES).
        "'OID -> "Solicitações de Manutenção
        LC_TEXTO = WA_SOLICITACOES-SE_RECORDID.
        PERFORM ADD_DOCUMENTO_TREE
           USING 'O' '' LC_CARGA-CARGA-ID_CARGA WA_SOLICITACOES-ID_SOLICITACAO WA_SOLICITACOES-SE_RECORDID 'OID' NODE ITEM LC_TEXTO ''
        CHANGING QTD_ITENS.
      ENDLOOP.
      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "WorkFlow de Aprovaçõe de Manutenções """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    CATCH ZCX_CARGA INTO DATA(EX_CARGA).
      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_ORDEM_CARREGAMENTO INTO DATA(EX_ORDEM).
      EX_ORDEM->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

  TRY .
      OBJ_CARGA->FREE( ).
    CATCH ZCX_CARGA.
  ENDTRY.

  CALL METHOD TREE->ADD_NODES_AND_ITEMS
    EXPORTING
      NODE_TABLE                     = NODE_TABLE
      ITEM_TABLE                     = ITEM_TABLE
      ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
    EXCEPTIONS
      FAILED                         = 1
      CNTL_SYSTEM_ERROR              = 3
      ERROR_IN_TABLES                = 4
      DP_ERROR                       = 5
      TABLE_STRUCTURE_NAME_NOT_FOUND = 6.

  TREE->EXPAND_NODES( EXPORTING NODE_KEY_TABLE = IT_NODE
    EXCEPTIONS
      FAILED                  = 1
      CNTL_SYSTEM_ERROR       = 2
      ERROR_IN_NODE_KEY_TABLE = 3
      DP_ERROR                = 4
      OTHERS                  = 5 ).

  SORT IT_TREE_INFO BY NODE_KEY ITEM_NAME.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  ADD_DOCUMENTO_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_NOTA_PO_NUMBER  text
*      -->P_1619   text
*----------------------------------------------------------------------*
FORM ADD_DOCUMENTO_TREE  USING P_TIPO_MOV      TYPE ZTP_MOV
                               P_CH_REFERENCIA TYPE ZCH_REF
                               P_DOCUMENTO
                               P_DOCUMENTO2
                               P_DOCUMENTO_REF
                               P_TIPO       TYPE CHAR03
                               NODE         TYPE TREEV_NODE
                               ITEM         TYPE MTREEITM
                               P_TEXTO      TYPE STRING
                               P_AV_VBELN   TYPE VBELN_VL
                      CHANGING P_QTD_ITENS  TYPE I.
  DATA: NODE_ TYPE TREEV_NODE,
        ITEM_ TYPE MTREEITM.

  CHECK P_DOCUMENTO IS NOT INITIAL.
  CHECK P_TIPO IS NOT INITIAL.

  ADD 1 TO P_QTD_ITENS.

  CLEAR: NODE_, ITEM_.
  NODE_-RELATKEY  = NODE-NODE_KEY.
  NODE_-RELATSHIP = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD.
  NODE_-NODE_KEY  = P_QTD_ITENS.

  CONDENSE NODE_-NODE_KEY NO-GAPS.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = NODE_-NODE_KEY
    IMPORTING
      OUTPUT = NODE_-NODE_KEY.

  IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
  IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
  IT_TREE_INFO-DOC_PART_1    = P_DOCUMENTO.
  IT_TREE_INFO-DOC_PART_2    = P_DOCUMENTO2.
  IT_TREE_INFO-DOC_REF       = P_DOCUMENTO_REF.
  IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
  IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN1.
  IT_TREE_INFO-TIPO          = P_TIPO.
  IT_TREE_INFO-AV_VBELN      = P_AV_VBELN.
  APPEND IT_TREE_INFO.

  ITEM_-NODE_KEY  = NODE_-NODE_KEY.
  ITEM_-ITEM_NAME = C_TREE-COLUMN1.
  ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK. " Text Item
  ITEM_-ALIGNMENT = CL_GUI_LIST_TREE=>ALIGN_AUTO.
  ITEM_-STYLE     = CL_GUI_LIST_TREE=>STYLE_DEFAULT.
  ITEM_-FONT      = CL_GUI_LIST_TREE=>ITEM_FONT_PROP.
  IF P_TEXTO IS NOT INITIAL.
    ITEM_-TEXT = P_TEXTO.
  ELSE.
    "ITEM_-T_IMAGE   = ICON_GOS_SERVICES.
    IF P_DOCUMENTO2 IS NOT INITIAL.
      CONCATENATE P_DOCUMENTO P_DOCUMENTO2 INTO ITEM_-TEXT SEPARATED BY '-'.
    ELSE.
      ITEM_-TEXT = P_DOCUMENTO.
    ENDIF.
  ENDIF.
  APPEND ITEM_ TO ITEM_TABLE.

  CASE P_TIPO.

      "Frete de Entrada
    WHEN 'FPO'. "Pedido de Compra
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FAV'. "Avido de Recebimento
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FDT'. "Documento de Transporte
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FP1'. "Pedágio REPOM
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FDC'. "Documento de Custo
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FOT'. "Ordem de Venda do Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FFT'. "Fatura do Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FFF'. "Documento Fiscal Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FCV'. "Contrato de Viagem
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'FMD'. "MDFE
      NODE_-N_IMAGE    = ICON_ORDER.

      "Entrada
    WHEN 'EPO'. "Pedido de Compra
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'EAV'. "Avido de Recebimento
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'EDM'. "Documento de Material
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'EFT'. "Documento de Faturamento
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'EFS'. "Documento Fiscal
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'EMS'. "Documento de Material - Sobra
      NODE_-N_IMAGE    = ICON_ORDER.

      "Saída
    WHEN 'SDM'. "Documento de Material Entrada Ajuste Sobra
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SPO'. "Pedido de Transferência
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SOV'. "Ordem de Venda da Mercadoria
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SRM'. "Remessa
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SVF'. "Fatura Da Mercadoria
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SFS'. "Documento Fiscal Mercadoria
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SDT'. "Documento de Transporte
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SP1'. "Pedágio REPOM
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SDC'. "Documento de Custo
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SOT'. "Ordem de Venda do Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SFT'. "Fatura do Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SFF'. "Documento Fiscal Frete
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SCV'. "Contrato de Viagem
      NODE_-N_IMAGE    = ICON_ORDER.
    WHEN 'SMD'. "MDFE
      NODE_-N_IMAGE    = ICON_ORDER.

      "Workflow de Aprovação
    WHEN 'OID'.
      NODE_-N_IMAGE    = ICON_WORKFLOW_ACTIVITY.
  ENDCASE.

  APPEND NODE_ TO NODE_TABLE.

  "Documento Fiscal Mercadoria
  IF P_TIPO EQ 'EFS' OR P_TIPO EQ 'SFS' OR P_TIPO EQ 'SFF' OR P_TIPO EQ 'FFF'.

    SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
      FROM J_1BNFDOC
     WHERE DOCNUM EQ @P_DOCUMENTO.

    IF WA_J_1BNFDOC-FORM IS NOT INITIAL.

      CLEAR: ITEM_, IT_TREE_INFO.
      ITEM_-NODE_KEY  = NODE_-NODE_KEY.
      ITEM_-ITEM_NAME = C_TREE-COLUMN2.
      ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
      CASE P_TIPO.
        WHEN 'FFF'.
          ITEM_-TEXT = 'DACTE'.
        WHEN 'EFS'.
          ITEM_-TEXT = 'DANFE'.
        WHEN 'SFS'.
          ITEM_-TEXT = 'DANFE'.
        WHEN 'SFF'.
          ITEM_-TEXT = 'DACTE'.
      ENDCASE.

      CLEAR: IT_TREE_INFO.
      IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
      IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
      IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
      IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
      IT_TREE_INFO-TIPO          = P_TIPO.
      IT_TREE_INFO-AV_VBELN      = P_AV_VBELN.

      CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
        EXPORTING
          DOC_NUMERO     = P_DOCUMENTO
          IMPRIMIR       = SPACE
        EXCEPTIONS
          NAO_LOCALIZADO = 1
          OTHERS         = 2.

      IF SY-SUBRC IS NOT INITIAL.
        ITEM_-T_IMAGE = ICON_BEN_OFFER_DEFAULT.
      ELSE.
        IT_TREE_INFO-DOC_PART_1 = P_DOCUMENTO.
        ITEM_-T_IMAGE = ICON_PRINT.
      ENDIF.
      APPEND IT_TREE_INFO.
      APPEND ITEM_ TO ITEM_TABLE.

    ENDIF.

  ENDIF.

  IF P_TIPO EQ 'SP1' OR P_TIPO EQ 'FP1'. "Pedágio REPOM
    CLEAR: ITEM_, IT_TREE_INFO.
    ITEM_-NODE_KEY  = NODE_-NODE_KEY.
    ITEM_-ITEM_NAME = C_TREE-COLUMN2.
    ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
    ITEM_-TEXT      = 'Pedágio'.
    ITEM_-T_IMAGE   = ICON_PRINT.
    APPEND ITEM_ TO ITEM_TABLE.

    CLEAR: IT_TREE_INFO.
    IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
    IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
    IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
    IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
    IT_TREE_INFO-TIPO          = P_TIPO.
    IT_TREE_INFO-DOC_REF       = P_DOCUMENTO_REF.
    IT_TREE_INFO-DOC_PART_1    = P_DOCUMENTO.
    IT_TREE_INFO-AV_VBELN      = P_AV_VBELN.
    APPEND IT_TREE_INFO.
  ENDIF.

  IF P_TIPO EQ 'SCV' OR P_TIPO EQ 'FCV'. "Contrato de Viagem
    CLEAR: ITEM_, IT_TREE_INFO.
    ITEM_-NODE_KEY  = NODE_-NODE_KEY.
    ITEM_-ITEM_NAME = C_TREE-COLUMN2.
    ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
    ITEM_-TEXT      = 'Viagem'.
    ITEM_-T_IMAGE   = ICON_PRINT.
    APPEND ITEM_ TO ITEM_TABLE.

    CLEAR: IT_TREE_INFO.
    IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
    IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
    IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
    IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
    IT_TREE_INFO-TIPO          = P_TIPO.
    IT_TREE_INFO-DOC_REF       = P_DOCUMENTO_REF.
    IT_TREE_INFO-DOC_PART_1    = P_DOCUMENTO.
    IT_TREE_INFO-AV_VBELN      = P_AV_VBELN.
    APPEND IT_TREE_INFO.
  ENDIF.

  IF P_TIPO EQ 'OID'.
    CLEAR: ITEM_, IT_TREE_INFO.
    ITEM_-NODE_KEY  = NODE_-NODE_KEY.
    ITEM_-ITEM_NAME = C_TREE-COLUMN2.
    ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
    ITEM_-TEXT      = 'SMR'.
    ITEM_-T_IMAGE   = ICON_WORKFLOW_WAIT_FOR_EVENTS.
    APPEND ITEM_ TO ITEM_TABLE.

    CLEAR: IT_TREE_INFO.
    IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
    IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
    IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
    IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
    IT_TREE_INFO-TIPO          = P_TIPO.
    IT_TREE_INFO-DOC_REF       = P_DOCUMENTO_REF.
    IT_TREE_INFO-DOC_PART_1    = P_DOCUMENTO. "Carga
    IT_TREE_INFO-DOC_PART_2    = P_DOCUMENTO2. "Solicitação
    IT_TREE_INFO-DOC_PART_3    = P_DOCUMENTO_REF. "Identificador SE
    APPEND IT_TREE_INFO.
  ENDIF.

  IF P_TIPO EQ 'SMD' OR P_TIPO EQ 'FMD'. "MDFE
    CLEAR: ITEM_, IT_TREE_INFO.
    ITEM_-NODE_KEY  = NODE_-NODE_KEY.
    ITEM_-ITEM_NAME = C_TREE-COLUMN2.
    ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_LINK.
    ITEM_-TEXT      = 'DAMDFE'.
    ITEM_-T_IMAGE   = ICON_PRINT.
    APPEND ITEM_ TO ITEM_TABLE.

    CLEAR: IT_TREE_INFO.
    IT_TREE_INFO-TP_MOVIMENTO  = P_TIPO_MOV.
    IT_TREE_INFO-CH_REFERENCIA = P_CH_REFERENCIA.
    IT_TREE_INFO-NODE_KEY      = NODE_-NODE_KEY.
    IT_TREE_INFO-ITEM_NAME     = C_TREE-COLUMN2.
    IT_TREE_INFO-TIPO          = P_TIPO.
    IT_TREE_INFO-DOC_REF       = P_DOCUMENTO.     "DOCNUM MDF-e
    IT_TREE_INFO-DOC_PART_1    = P_DOCUMENTO_REF. "Número da MDF-e
    IT_TREE_INFO-AV_VBELN      = P_AV_VBELN. "Número da MDF-e
    APPEND IT_TREE_INFO.
  ENDIF.

  CLEAR: ITEM_.
  ITEM_-NODE_KEY  = NODE_-NODE_KEY.
  ITEM_-ITEM_NAME = C_TREE-COLUMN3.
  ITEM_-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
  CASE P_TIPO.

      "Frete de Entrada
    WHEN 'FPO'.
      ITEM_-TEXT = 'Pedido de Compra'.
    WHEN 'FAV'.
      ITEM_-TEXT = 'Aviso de Recebimento'.
    WHEN 'FDT'.
      ITEM_-TEXT = 'Documento de Transporte'.
    WHEN 'FP1'.
      ITEM_-TEXT = 'Pedágio REPOM'.
    WHEN 'FDC'.
      ITEM_-TEXT = 'Documento de Custo'.
    WHEN 'FOT'.
      ITEM_-TEXT = 'Ordem de Venda do Frete'.
    WHEN 'FFT'.
      ITEM_-TEXT = 'Fatura do Frete'.
    WHEN 'FFF'.
      ITEM_-TEXT = 'CT-e'.
    WHEN 'FCV'.
      ITEM_-TEXT = 'Contrato de Viagem'.
    WHEN 'FMD'. "MDFE
      ITEM_-TEXT = 'MDFE'.

      "Entrada
    WHEN 'EPO'.
      ITEM_-TEXT = 'Pedido de Compra'.
    WHEN 'EAV'.
      ITEM_-TEXT = 'Aviso de Recebimento'.
    WHEN 'EDM'.
      ITEM_-TEXT = 'Documento de Material'.
    WHEN 'EFT'.
      ITEM_-TEXT = 'Documento de Faturamento'.
    WHEN 'EFS'.
      ITEM_-TEXT = 'Documento Fiscal'.
    WHEN 'EMS'.
      ITEM_-TEXT = 'Documento de Material Sobra'.

      "Saída
    WHEN 'SDM'.
      ITEM_-TEXT = 'Documento de Material Sobra'.
    WHEN 'SPO'.
      ITEM_-TEXT = 'Pedido de Compra - Transferência'.
    WHEN 'SOV'.
      ITEM_-TEXT = 'Ordem de Venda da Mercadoria'.
    WHEN 'SRM'.
      ITEM_-TEXT = 'Remessa'.
    WHEN 'SVF'.
      ITEM_-TEXT = 'Fatura Da Mercadoria'.
    WHEN 'SFS'.
      ITEM_-TEXT = 'NF-e'.
    WHEN 'SDT'.
      ITEM_-TEXT = 'Documento de Transporte'.
    WHEN 'SP1'.
      ITEM_-TEXT = 'Pedágio REPOM'.
    WHEN 'SDC'.
      ITEM_-TEXT = 'Documento de Custo'.
    WHEN 'SOT'.
      ITEM_-TEXT = 'Ordem de Venda do Frete'.
    WHEN 'SFT'.
      ITEM_-TEXT = 'Fatura do Frete'.
    WHEN 'SFF'.
      ITEM_-TEXT = 'CT-e'.
    WHEN 'SCV'.
      ITEM_-TEXT = 'Contrato de Viagem'.
    WHEN 'SMD'. "MDFE
      ITEM_-TEXT = 'MDFE'.

      "Workflow de Aprovação
    WHEN 'OID'.
      ITEM_-TEXT = 'Solicitação de Manutenção'.
  ENDCASE.
  CHECK ITEM_-TEXT IS NOT INITIAL.
  APPEND ITEM_ TO ITEM_TABLE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_INFO_NODE_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
FORM MOSTRA_INFO_NODE_ITEM  USING P_NODE_KEY TYPE  TV_NODEKEY P_ITEM_NAME TYPE  TV_ITMNAME.

  DATA: P_FISCAL           TYPE J_1BDOCNUM,
        P_BELNR            TYPE RE_BELNR,
        P_GJAHR            TYPE GJAHR,
        P_MBLNR            TYPE MBLNR,
        P_MJAHR            TYPE MJAHR,
        P_VBELN            TYPE VBELN_VL,
        P_EBELN            TYPE EBELN,
        P_TKNUM            TYPE TKNUM,
        P_FKNUM            TYPE FKNUM,
        P_NMDFE            TYPE J_1BNFNUM9,
        P_ID_PROC_CLIENTE  TYPE  ZDE_ID_PROC_CLIENTE,
        P_ID_CARGA         TYPE  ZDE_ID_CARGA,
        O_ID_SOLICITACAO   TYPE ZDE_ID_SOL_AJUSTE,
        P_SE_IDENTIFICADOR TYPE ZDE_SE_RECORDID_WORKFLOW.

  READ TABLE IT_TREE_INFO INTO DATA(WA_INFO_TRE)
    WITH KEY NODE_KEY = P_NODE_KEY ITEM_NAME = P_ITEM_NAME BINARY SEARCH.

  IF SY-SUBRC IS INITIAL.

    CASE WA_INFO_TRE-ITEM_NAME.
      WHEN C_TREE-COLUMN1.
        CASE WA_INFO_TRE-TIPO.

          WHEN 'FPO'. "Pedido de Compra
            P_EBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_PEDIDO USING P_EBELN.
          WHEN 'FAV'. "Avido de Recebimento
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_AVISO USING P_VBELN.
          WHEN 'FDT'. "Documento de Transporte
            P_TKNUM = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_TRANSPORTE USING P_TKNUM.
          WHEN 'FP1'. "Pedágio REPOM
            P_ID_PROC_CLIENTE = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM MOSTRAR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE.
          WHEN 'FDC'. "Documento de Custo
            P_FKNUM = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_CUSTO USING P_FKNUM.
          WHEN 'FOT'. "Ordem de Venda Frete
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_ORDEM_VENDA USING P_VBELN.
          WHEN 'FFT'. "Fatura do Frete
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_FATURA_VF USING P_VBELN.
          WHEN 'FFF'. "Documento Fiscal Frete
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_FISCAL USING P_FISCAL.

          WHEN 'EPO'. "Pedido de Compra
            P_EBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_PEDIDO USING P_EBELN.
          WHEN 'EAV'. "Avido de Recebimento
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_AVISO USING P_VBELN.
          WHEN 'EDM'. "Documento de Material
            P_MBLNR = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_MJAHR = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            PERFORM MOSTRAR_DOC_MATERIAL USING P_MBLNR P_MJAHR.
          WHEN 'EMS'. "Documento de Material - Sobra
            P_MBLNR = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_MJAHR = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            PERFORM MOSTRAR_DOC_MATERIAL USING P_MBLNR P_MJAHR.
          WHEN 'EFT'. "Documento de Faturamento
            P_BELNR = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_GJAHR = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            PERFORM MOSTRAR_FATURA USING P_BELNR P_GJAHR.
          WHEN 'EFS'. "Documento Fiscal
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_FISCAL USING P_FISCAL.
          WHEN 'SDM'. "Documento de Material Sobra
            P_MBLNR = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_MJAHR = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            PERFORM MOSTRAR_DOC_MATERIAL USING P_MBLNR P_MJAHR.
          WHEN 'SPO'. "Pedido de Compra de Transferencia
            P_EBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_PEDIDO USING P_EBELN.
          WHEN 'SOV'. "Ordem de Venda Mercadoria
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_ORDEM_VENDA USING P_VBELN.
          WHEN 'SRM'. "Remessa
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_REMESSA USING P_VBELN.
          WHEN 'SVF'. "Fatura Da Mercadoria
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_FATURA_VF USING P_VBELN.
          WHEN 'SFS'. "Documento Fiscal Mercadoria
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_FISCAL USING P_FISCAL.
          WHEN 'SDT'. "Documento de Transporte
            P_TKNUM = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_TRANSPORTE USING P_TKNUM.
          WHEN 'SP1'. "Pedágio REPOM
            P_ID_PROC_CLIENTE = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM MOSTRAR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE.
          WHEN 'SDC'. "Documento de Custo
            P_FKNUM = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_CUSTO USING P_FKNUM.
          WHEN 'SOT'. "Ordem de Venda Frete
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_ORDEM_VENDA USING P_VBELN.
          WHEN 'SFT'. "Fatura do Frete
            P_VBELN = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_FATURA_VF USING P_VBELN.
          WHEN 'SFF'. "Documento Fiscal Frete
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM MOSTRAR_DOC_FISCAL USING P_FISCAL.
          WHEN 'OID'. "Solicitação de Manutenção
            P_ID_CARGA = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_ID_PROC_CLIENTE = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            PERFORM MOSTRAR_SOLICITACAO_MANUT USING P_ID_CARGA P_ID_PROC_CLIENTE.
        ENDCASE.
      WHEN C_TREE-COLUMN2.
        CASE WA_INFO_TRE-TIPO.

            "Frete de Entrada
          WHEN 'FP1'. "Pedágio REPOM
            P_ID_PROC_CLIENTE = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE.
          WHEN 'FFF'. "Documento Fiscal Frete
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM IMPRIMIR_DOC_FISCAL USING P_FISCAL.
          WHEN 'FCV'. "Contrato de Viagem Administradora
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_CTR_VIAGEM USING P_FISCAL.
          WHEN 'FMD'. "MDFE
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_NMDFE  = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_MDFE USING P_NMDFE P_FISCAL.

            "Entrada
          WHEN 'ERO'. "Romaneio de Entrada (PDF)
            PERFORM IMPRIMIR_ROMANEIO USING WA_INFO_TRE-CH_REFERENCIA.
          WHEN 'EFS'. "Documento Fiscal Entrada Própria
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM IMPRIMIR_DOC_FISCAL USING P_FISCAL.

            "Saída
          WHEN 'SRO'. "Romaneio de Saída (PDF)
            PERFORM IMPRIMIR_ROMANEIO USING WA_INFO_TRE-CH_REFERENCIA.
          WHEN 'SFS'. "Documento Fiscal Mercadoria
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM IMPRIMIR_DOC_FISCAL USING P_FISCAL.
          WHEN 'SP1'. "Pedágio REPOM
            P_ID_PROC_CLIENTE = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE.
          WHEN 'SFF'. "Documento Fiscal Frete
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            PERFORM IMPRIMIR_DOC_FISCAL USING P_FISCAL.
          WHEN 'SCV'. "Contrato de Viagem Administradora
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_CTR_VIAGEM USING P_FISCAL.
          WHEN 'SMD'. "MDFE
            CHECK WA_INFO_TRE-DOC_PART_1 IS NOT INITIAL.
            P_NMDFE  = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_FISCAL = CONV #( WA_INFO_TRE-DOC_REF ).
            PERFORM IMPRIMIR_MDFE USING P_NMDFE P_FISCAL.

            "Workflow de Aprovação
          WHEN 'OID'. "Solicitação de Manutenção
            P_ID_CARGA = CONV #( WA_INFO_TRE-DOC_PART_1 ).
            P_ID_PROC_CLIENTE  = CONV #( WA_INFO_TRE-DOC_PART_2 ).
            P_SE_IDENTIFICADOR = CONV #( WA_INFO_TRE-DOC_PART_3 ).
            PERFORM MOSTRAR_SOLICITACAO_MANUT_SE USING P_ID_CARGA P_ID_PROC_CLIENTE P_SE_IDENTIFICADOR.
        ENDCASE.
      WHEN C_TREE-COLUMN3.
        CASE WA_INFO_TRE-TIPO.
          WHEN 'ETK'. "Romaneio de Entrada (Romaneio)
            PERFORM IMPRIMIR_TICKET USING WA_INFO_TRE-CH_REFERENCIA.
          WHEN 'STK'. "Romaneio de Saída (Romaneio)
            PERFORM IMPRIMIR_TICKET USING WA_INFO_TRE-CH_REFERENCIA.
        ENDCASE.
    ENDCASE.

  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_FISCAL  USING P_FISCAL TYPE J_1BDOCNUM.

  DATA: GF_NFOBJN LIKE J_1BINTERF-NFOBJN.

  CHECK P_FISCAL IS NOT INITIAL.

  CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
    EXPORTING
      DOC_NUMBER         = P_FISCAL
    IMPORTING
      OBJ_NUMBER         = GF_NFOBJN
    EXCEPTIONS
      DOCUMENT_NOT_FOUND = 1
      DOCUM_LOCK         = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
    EXPORTING
      OBJ_NUMBER         = GF_NFOBJN
    EXCEPTIONS
      OBJECT_NOT_FOUND   = 1
      SCR_CTRL_NOT_FOUND = 2
      OTHERS             = 3.

  CALL FUNCTION 'J_1B_NF_OBJECT_DROP'
    EXPORTING
      OBJ_NUMBER       = GF_NFOBJN
    EXCEPTIONS
      OBJECT_NOT_FOUND = 1
      OTHERS           = 2.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM MOSTRAR_MONITOR_ELETRONICO  USING P_FISCAL TYPE J_1BDOCNUM .

  DATA: GF_NFOBJN LIKE J_1BINTERF-NFOBJN.

  CHECK P_FISCAL IS NOT INITIAL.

  SELECT SINGLE * INTO @DATA(WA_J_1BNFDOC)
    FROM J_1BNFDOC
   WHERE DOCNUM EQ @P_FISCAL.

  SET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD WA_J_1BNFDOC-DOCNUM.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD WA_J_1BNFDOC-BUKRS.

  IF WA_J_1BNFDOC-FORM IS NOT INITIAL.
    CASE WA_J_1BNFDOC-MODEL.
      WHEN '55'.
        CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.
      WHEN '57'.
        CALL TRANSACTION 'ZCTE' AND SKIP FIRST SCREEN.
      WHEN '58'.
        CALL TRANSACTION 'ZMDFE' AND SKIP FIRST SCREEN.
    ENDCASE.
  ELSE.
    CASE WA_J_1BNFDOC-MODEL.
      WHEN '55'.
        CALL TRANSACTION 'ZNFE_TERC' AND SKIP FIRST SCREEN.
      WHEN '57'.
        CALL TRANSACTION 'ZCTE_TERC' AND SKIP FIRST SCREEN.
    ENDCASE.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DOC_FISCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM IMPRIMIR_DOC_FISCAL  USING P_FISCAL TYPE J_1BDOCNUM.

  CHECK P_FISCAL IS NOT INITIAL.

  CALL FUNCTION 'Z_SD_PRINT_NFE_CTE'
    EXPORTING
      DOC_NUMERO     = P_FISCAL
    EXCEPTIONS
      NAO_LOCALIZADO = 1
      OTHERS         = 2.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_FISCAL

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_CTR_VIAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_CTE_ALV_  text
*----------------------------------------------------------------------*
FORM IMPRIMIR_CTR_VIAGEM USING P_FISCAL TYPE J_1BDOCNUM.

  CHECK P_FISCAL IS NOT INITIAL.

  CALL FUNCTION 'Z_SD_IMPRIMIR_CTR_CIOT'
    EXPORTING
      P_CTE_AVULSO = P_FISCAL
    EXCEPTIONS
      NAO_CIOT     = 1
      ERRO_STATUS  = 2
      ERROR        = 3
      OTHERS       = 4.

  IF SY-SUBRC IS NOT INITIAL.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " IMPRIMIR_CTR_VIAGEM

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA
*&---------------------------------------------------------------------*
FORM MOSTRAR_FATURA  USING    P_BELNR TYPE RE_BELNR
                              P_GJAHR TYPE GJAHR.
  IF P_BELNR IS NOT INITIAL AND P_GJAHR IS NOT INITIAL.
    SET PARAMETER ID 'RBN' FIELD P_BELNR.
    SET PARAMETER ID 'GJR' FIELD P_GJAHR.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.                    " MOSTRAR_FATURA

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_MATERIAL  USING P_MBLNR TYPE MBLNR
                                 P_MJAHR TYPE MJAHR.

  IF P_MBLNR IS NOT INITIAL.
    SET PARAMETER ID 'MBN' FIELD P_MBLNR.
    SET PARAMETER ID 'MJA' FIELD P_MJAHR.
    CALL TRANSACTION 'MIGO_GO' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_AVISO USING P_VBELN TYPE VBELN_VL.

  IF P_VBELN IS NOT INITIAL.
    SET PARAMETER ID 'VL'  FIELD P_VBELN.
    SET PARAMETER ID 'VLM' FIELD P_VBELN.
    CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_AVISO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_REMESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_REMESSA USING P_VBELN TYPE VBELN_VL.

  IF P_VBELN IS NOT INITIAL.
    SET PARAMETER ID 'VL'  FIELD P_VBELN.
    CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_REMESSA

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDIDO
*&---------------------------------------------------------------------*
FORM MOSTRAR_PEDIDO  USING  P_EBELN TYPE EBELN.

  IF P_EBELN IS NOT INITIAL.
    SET PARAMETER ID 'BES' FIELD P_EBELN.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_PEDIDO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_FATURA_VF
*&---------------------------------------------------------------------*
FORM MOSTRAR_FATURA_VF USING P_VBELN TYPE VBELN_VF.

  IF P_VBELN IS NOT INITIAL.
    SET PARAMETER ID 'VF'    FIELD P_VBELN.
    CALL TRANSACTION 'VF03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_FATURA_VF

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_SOLICITACAO_MANUT
*&---------------------------------------------------------------------*
FORM MOSTRAR_SOLICITACAO_MANUT USING P_ID_CARGA TYPE ZDE_ID_CARGA P_ID_SOLICITACAO TYPE ZDE_ID_SOL_AJUSTE.

  IF P_ID_SOLICITACAO IS NOT INITIAL AND P_ID_CARGA IS NOT INITIAL.
    SUBMIT ZMMR126_0001
                   WITH PCK_CAD  EQ ABAP_TRUE
                   WITH PMANUT   EQ ABAP_TRUE
                   WITH PSAFRA   EQ PSAFRA
                   WITH PEMPRE   EQ PEMPRE
                   WITH PFILIA   EQ PFILIA
                   WITH PIDCARGA EQ P_ID_CARGA
                   WITH PIDSOLIC EQ P_ID_SOLICITACAO AND RETURN.
  ENDIF.

ENDFORM.                    " MOSTRAR_SOLICITACAO_MANUT

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_SOLICITACAO_MANUT_se
*&---------------------------------------------------------------------*
FORM MOSTRAR_SOLICITACAO_MANUT_SE USING P_ID_CARGA TYPE ZDE_ID_CARGA P_ID_SOLICITACAO TYPE ZDE_ID_SOL_AJUSTE P_SE_IDENTIFICADOR TYPE ZDE_SE_RECORDID_WORKFLOW.

  IF P_ID_SOLICITACAO IS NOT INITIAL AND P_ID_CARGA IS NOT INITIAL AND P_SE_IDENTIFICADOR IS NOT INITIAL.

  ENDIF.

ENDFORM.                    " MOSTRAR_SOLICITACAO_MANUT_SE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_TRANSPORTE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_TRANSPORTE USING P_TKNUM TYPE TKNUM.

  IF P_TKNUM IS NOT INITIAL.
    SET PARAMETER ID 'TNR' FIELD P_TKNUM.
    CALL TRANSACTION 'VT03N' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_TRANSPORTE

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_PEDAGIO_REPOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE TYPE  ZDE_ID_PROC_CLIENTE.

  IF P_ID_PROC_CLIENTE IS NOT INITIAL.
    CALL FUNCTION 'Z_REPOM_CADASTRO_PEDAGIO'
      EXPORTING
        I_CONSULTA        = ABAP_TRUE
        I_ID_PROC_CLIENTE = P_ID_PROC_CLIENTE.
  ENDIF.

ENDFORM.                    " MOSTRAR_PEDAGIO_REPOM

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_PEDAGIO_REPOM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM IMPRIMIR_PEDAGIO_REPOM USING P_ID_PROC_CLIENTE TYPE  ZDE_ID_PROC_CLIENTE.

  IF P_ID_PROC_CLIENTE IS NOT INITIAL.

    ZCL_REPOM_VIAGEM_VPR=>IMPRIMIR_VIAGEM(
      EXPORTING
        I_ID_PROC_CLIENTE = P_ID_PROC_CLIENTE
      EXCEPTIONS
        ERRO              = 1
        OTHERS            = 2 ).

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDFORM.                    " IMPRIMIR_PEDAGIO_REPOM

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_DOC_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM MOSTRAR_DOC_CUSTO USING P_FKNUM TYPE FKNUM.

  IF P_FKNUM IS NOT INITIAL.
    SET PARAMETER ID 'FKK'    FIELD P_FKNUM.
    CALL TRANSACTION 'VI03' AND SKIP FIRST SCREEN.
  ENDIF.

ENDFORM.                    " MOSTRAR_DOC_CUSTO

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_ORDEM_VENDA
*&---------------------------------------------------------------------*
FORM MOSTRAR_ORDEM_VENDA USING P_VBELN TYPE VBELN_VA.
  ZCL_ORDEM_VENDA=>ZIF_ORDEM_VENDA~OPEN( P_VBELN ).
ENDFORM.                    " MOSTRAR_DOC_CUSTO

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_MDFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_NMDFE  text
*      -->P_P_FISCAL  text
*----------------------------------------------------------------------*
FORM IMPRIMIR_MDFE  USING  P_NMDFE  TYPE J_1BNFNUM9
                           P_FISCAL TYPE J_1BDOCNUM.

  DATA: LC_MDFE TYPE REF TO ZCL_MDFE.

  "CHECK P_NMDFE IS NOT INITIAL.
  CHECK P_FISCAL IS NOT INITIAL.

  CREATE OBJECT LC_MDFE
    EXPORTING
      I_NMDFE  = P_NMDFE
      I_DOCNUM = P_FISCAL.

  LC_MDFE->PRINT_MDFE( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_TICKET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_INFO_TRE_CH_REFERENCIA  text
*----------------------------------------------------------------------*
FORM IMPRIMIR_TICKET  USING P_CH_ROMANEIO TYPE ZCH_REF.

  DATA: OBJ_ROMANEIO TYPE REF TO ZCL_ROMANEIO.

  TRY .
      CREATE OBJECT OBJ_ROMANEIO.
      OBJ_ROMANEIO->SET_REGISTRO( I_ID_REGISTRO = P_CH_ROMANEIO ).
      CHECK SY-SUBRC IS INITIAL.
      OBJ_ROMANEIO->IMPRIMIR_TICKET( ).
    CATCH ZCX_CADASTRO INTO DATA(LC_EX_CADASTRO).
      LC_EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_ROMANEIO INTO DATA(LC_EX_ROMANEIO).
      LC_EX_ROMANEIO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

  OBJ_ROMANEIO->LIMPAR_REGISTRO( ).
  CLEAR: OBJ_ROMANEIO.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_ROMANEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_INFO_TRE_CH_REFERENCIA  text
*----------------------------------------------------------------------*
FORM IMPRIMIR_ROMANEIO  USING P_CH_ROMANEIO TYPE ZCH_REF.

  DATA: OBJ_ROMANEIO TYPE REF TO ZCL_ROMANEIO.

  TRY .
      CREATE OBJECT OBJ_ROMANEIO.
      OBJ_ROMANEIO->SET_REGISTRO( I_ID_REGISTRO = P_CH_ROMANEIO ).
      CHECK SY-SUBRC IS INITIAL.
      OBJ_ROMANEIO->IMPRIMIR_PRD( ).
    CATCH ZCX_CADASTRO INTO DATA(LC_EX_CADASTRO).
      LC_EX_CADASTRO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
    CATCH ZCX_ROMANEIO INTO DATA(LC_EX_ROMANEIO).
      LC_EX_ROMANEIO->PUBLISHED_ERRO( I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
  ENDTRY.

  CLEAR: OBJ_ROMANEIO.

ENDFORM.
