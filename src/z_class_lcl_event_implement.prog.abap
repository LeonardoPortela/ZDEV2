*&-------------------------------------------------------------------*
*&  CLASS LCL_EVENT_HANDLER IMPLEMENTATION                           *
*&  AUTOR: ENIO JESUS                                                *
*&  13.07.2015                                                       *
*&-------------------------------------------------------------------*

**********************************************************************
*& Descrição: Registrar evento de click no checkbox                 &*
*& Parâmetro: ER_DATA_CHANGED->                                     &*
*& Atributos Globais                                                &*
********************************************************************&*
 METHOD CBX_DATA_CHANGED.
   LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS INTO LS_GOOD.

     CASE LS_GOOD-FIELDNAME.
       WHEN 'CBX_EMPRESTAR'.
         REFRESH IT_SAIDA_EMPRESTIMO_EQUI.
         DELETE ER_DATA_CHANGED->MT_GOOD_CELLS WHERE VALUE NE 'X'.

         READ TABLE IT_SAIDA_EQUI_DISPONIVEIS INTO WA_SAIDA_EQUI_DISPONIVEIS INDEX LS_GOOD-ROW_ID.
         WA_SAIDA_EMPRESTIMO_EQUI-EQUNR         = WA_SAIDA_EQUI_DISPONIVEIS-EQUNR.
         WA_SAIDA_EMPRESTIMO_EQUI-IWERK         = WA_SAIDA_EQUI_DISPONIVEIS-IWERK.
         WA_SAIDA_EMPRESTIMO_EQUI-EQKTX         = WA_SAIDA_EQUI_DISPONIVEIS-EQKTX.
         WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_REMON = 'X'.
         WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST = ''.
         APPEND WA_SAIDA_EMPRESTIMO_EQUI TO IT_SAIDA_EMPRESTIMO_EQUI.

       WHEN 'CBX_ORD_ABAST'.
         READ TABLE IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI
              INDEX LS_GOOD-ROW_ID.

         WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST = LS_GOOD-VALUE.
         MODIFY IT_SAIDA_EMPRESTIMO_EQUI FROM WA_SAIDA_EMPRESTIMO_EQUI INDEX LS_GOOD-ROW_ID.

       WHEN 'CBX_DEVOLVER'.
         READ TABLE IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL
              INDEX LS_GOOD-ROW_ID.

         WA_SAIDA_EQUI_RESPONSAVEL-CBX_DEVOLVER = LS_GOOD-VALUE.
         MODIFY IT_SAIDA_EQUI_RESPONSAVEL FROM WA_SAIDA_EQUI_RESPONSAVEL INDEX LS_GOOD-ROW_ID.

       WHEN 'DT_DEVOLUCAO'.
         READ TABLE IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL
              INDEX LS_GOOD-ROW_ID.

         WA_SAIDA_EQUI_RESPONSAVEL-DT_DEVOLUCAO = LS_GOOD-VALUE.
         MODIFY IT_SAIDA_EQUI_RESPONSAVEL FROM WA_SAIDA_EQUI_RESPONSAVEL INDEX LS_GOOD-ROW_ID.

       WHEN 'HR_DEVOLUCAO'.
         READ TABLE IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL
              INDEX LS_GOOD-ROW_ID.

         WA_SAIDA_EQUI_RESPONSAVEL-HR_DEVOLUCAO = LS_GOOD-VALUE.
         MODIFY IT_SAIDA_EQUI_RESPONSAVEL FROM WA_SAIDA_EQUI_RESPONSAVEL INDEX LS_GOOD-ROW_ID.
     ENDCASE.

   ENDLOOP.

   CLEAR: WA_SAIDA_EQUI_RESPONSAVEL,
          WA_SAIDA_EQUI_DISPONIVEIS,
          WA_SAIDA_EQUI_EMPRESTADOS,
          LS_GOOD.

   REFRESH: ER_DATA_CHANGED->MT_GOOD_CELLS.

 ENDMETHOD.                    "ON_DATA_CHANGED_CHECKBOX

**********************************************************************
*& Descrição: Criar botões em ALV                                   &*
*& Parâmetro: E_OBJECT->                                            &*
*& Atributos Globais                                                &*
********************************************************************&*
 METHOD SET_TOOLBAR.
   CLEAR: WA_TOOLBAR, E_OBJECT->MT_TOOLBAR.
   CASE SY-DYNNR.
*    Botão <Emprestar equipamentos> Tela 0100;
     WHEN 0110.
       CLEAR WA_TOOLBAR.
       WA_TOOLBAR-BUTN_TYPE = 3.
       APPEND WA_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
       CLEAR WA_TOOLBAR.
       WA_TOOLBAR-FUNCTION     = 'BTN_EMPRESTAR_EQUI'.
       WA_TOOLBAR-ICON         =  ICON_DELIVERY.
       WA_TOOLBAR-QUICKINFO    = 'Emprestar Equipamentos'.
       WA_TOOLBAR-BUTN_TYPE    = 0.
       WA_TOOLBAR-TEXT         = 'Emprestar Equipamentos'.
       APPEND WA_TOOLBAR TO E_OBJECT->MT_TOOLBAR.

     WHEN 0130.
       CLEAR WA_TOOLBAR.
       WA_TOOLBAR-BUTN_TYPE = 3.
       APPEND WA_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
       CLEAR WA_TOOLBAR.
       WA_TOOLBAR-FUNCTION     = 'BTN_DEVOLVER_EQUI'.
       WA_TOOLBAR-ICON         =  ICON_TRANSPORTATION_MODE.
       WA_TOOLBAR-QUICKINFO    = 'Devolver Equipamentos'.
       WA_TOOLBAR-BUTN_TYPE    = 0.
       WA_TOOLBAR-TEXT         = 'Devolver Equipamentos'.
       APPEND WA_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
   ENDCASE.
 ENDMETHOD.                    "SET_TOOLBAR

**********************************************************************
*& Descrição: Registra ação nos botões da ALV                       &*
*& Parâmetro: E_UCOMM                                               &*
*& Atributos Globais                                                &*
********************************************************************&*
 METHOD GET_UCOMM.
   DATA: R_CHECAR_INFO_EMPRESTIMO  TYPE REF TO ZUTEIS,
         R_INICIAR_PROCESSO_ZBAPIS TYPE REF TO ZBAPIS.

   CREATE OBJECT: R_CHECAR_INFO_EMPRESTIMO,
                  R_INICIAR_PROCESSO_ZBAPIS.

   CASE E_UCOMM.

*    Registra ação no botão 'Emprestar Equipamentos' Tela 0100.
*    RETURN_STATUS é um atributo global que retorna se a operação não foi bem sucedida.

     WHEN 'BTN_EMPRESTAR_EQUI'.
       IF ( IT_SAIDA_EMPRESTIMO_EQUI IS NOT INITIAL ).

         R_CHECAR_INFO_EMPRESTIMO->Z_CHECAR_EQUI_HIERARCHY( IMPORTING
                                                            RETURN = RETURN_STATUS ).
         CHECK RETURN_STATUS IS INITIAL.
         CALL SCREEN 0200 STARTING AT  1  1
                            ENDING AT 95 16.
       ELSE.
         MESSAGE I836(SD) WITH TEXT-001.
       ENDIF.

     WHEN 'BTN_DEVOLVER_EQUI'.
       IF ( IT_SAIDA_EQUI_RESPONSAVEL IS NOT INITIAL ).

         R_CHECAR_INFO_EMPRESTIMO->Z_CHECAR_DT_HR_DEVOLUCAO( IMPORTING
                                                             RETURN = RETURN_STATUS
                                                             CHANGING
                                                             IT_TAB = IT_SAIDA_EQUI_RESPONSAVEL
                                                             WA_TAB = WA_SAIDA_EQUI_RESPONSAVEL ).
         CHECK RETURN_STATUS IS INITIAL.
         R_INICIAR_PROCESSO_ZBAPIS->Z_INICIAR_PROCESSO_DEVOLUCAO( ).

       ELSE.
         MESSAGE I836(SD) WITH TEXT-001.
       ENDIF.

   ENDCASE.
 ENDMETHOD.                    "GET_UCOMM
