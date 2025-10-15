*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Camila Brand                                            &*
*& Data.....: 10.10.2022                                              &*
*& Descrição: Cockpit - Equivalência Patrimonial                      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT zglr076.

TABLES: zglt0107, zglt0108, sscrfields.

TYPE-POOLS: icon.

CONSTANTS:
 c_btlog(03) TYPE c VALUE 'EXL'.

*---------------------------------------------------------------------*
* DATA                                                                *
*---------------------------------------------------------------------*
DATA: l_sel_button TYPE smp_dyntxt,
      l_dt_sel     TYPE sy-datum.

DATA:
  git_dta TYPE TABLE OF bdcdata,
  opt     TYPE ctu_params,
  git_msg TYPE TABLE OF bdcmsgcoll.

DATA: gva_ucomm TYPE sy-ucomm.
DATA: git_ucomm TYPE TABLE OF sy-ucomm.
DATA: it_screen_status TYPE TABLE OF sy-ucomm.

*&--------------------------------------------------------------------&*
*& Tela de Seleção                                                    &*
*&--------------------------------------------------------------------&*

"Begin of CS2023000082  #103662 FF   21.02.2023
*SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
*  SELECTION-SCREEN PUSHBUTTON 01(20)  bt01 USER-COMMAND fc01.
*  SELECTION-SCREEN PUSHBUTTON 23(25)  bt02 USER-COMMAND fc02.
*  SELECTION-SCREEN PUSHBUTTON 50(27)  bt05 USER-COMMAND fc05.
*  SELECTION-SCREEN PUSHBUTTON 79(28)  bt06 USER-COMMAND fc06.
*SELECTION-SCREEN: END OF BLOCK b2.

**SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-002.
**  SELECTION-SCREEN PUSHBUTTON 01(28)  bt03 USER-COMMAND fc03.
**SELECTION-SCREEN: END OF BLOCK b3.
**SELECTION-SCREEN SKIP.
**  End of FF  21.02.2023


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-003.

  PARAMETERS: p_invra TYPE zglt0107-investidora,
              p_invda TYPE zglt0107-investida,
              p_monat TYPE zglt0107-monat,
              p_gjahr TYPE zglt0107-gjahr.

SELECTION-SCREEN: END OF BLOCK b1.


**  Begin of CS2023000082  #103662 FF   21.02.2023 - alteardo, pois só é possível 5 function keys
*SELECTION-SCREEN FUNCTION KEY 1.  "Will have a function code of 'FC01'
*SELECTION-SCREEN FUNCTION KEY 2.  "Will have a function code of 'FC02'
*SELECTION-SCREEN FUNCTION KEY 3.  "Will have a function code of 'FC03'
*SELECTION-SCREEN FUNCTION KEY 4.  "Will have a function code of 'FC04'
*SELECTION-SCREEN FUNCTION KEY 5.  "Will have a function code of 'FC05'
**  End of FF  21.02.2023


AT SELECTION-SCREEN OUTPUT.

**  Begin of CS2023000082  #103662 FF   21.02.2023
***  CONCATENATE icon_submit   TEXT-b01 INTO bt01. "Cadastro Empresa
***  CONCATENATE icon_submit   TEXT-b02 INTO bt02. "Parametro Contabilizaçào
**  CONCATENATE icon_simulate TEXT-b03 INTO bt03. "Equivalência Patrimonial
**  CONCATENATE icon_simulate TEXT-b04 INTO bt04. "Conversão do Balanço
***  CONCATENATE icon_submit   TEXT-b05 INTO bt05. "Vlr. Patrimônio Líquido
***  CONCATENATE icon_submit   TEXT-b06 INTO bt06. "Parametrizar Reflexa PL
**  End of FF  21.02.2023

*  gva_ucomm = 'ONLI'.
*  APPEND gva_ucomm TO git_ucomm.
*  CLEAR gva_ucomm.
*
*  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*    EXPORTING
*      p_status  = sy-pfkey
**     P_PROGRAM = ' '
*    TABLES
*      p_exclude = git_ucomm.


it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).

  IF sy-dynnr = 1000.

    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status  = 'ST1000' "SY-pfkey "'ST1000'
        p_program = SY-REPID

      TABLES
        p_exclude = it_screen_status
        .

  ENDIF.


INITIALIZATION.

**  Begin of CS2023000082  #103662 FF   21.02.2023
*  l_sel_button-icon_id   = icon_submit.
*  l_sel_button-icon_text = 'Cadastro Empresa'.
*  sscrfields-functxt_01  = l_sel_button.
*
*  l_sel_button-icon_id   = icon_submit.
*  l_sel_button-icon_text = 'Parâmetro Contabilização'.
*  sscrfields-functxt_02  = l_sel_button.

*  l_sel_button-icon_id   = icon_simulate.
*  l_sel_button-icon_text = 'Equivalência Patrimonial'.
*  sscrfields-functxt_03  = l_sel_button.
*
*  l_sel_button-icon_id   = icon_simulate.
*  l_sel_button-icon_text = 'Conversão do Balanço'.
*  sscrfields-functxt_04  = l_sel_button.
*
*  l_sel_button-icon_id   = icon_simulate.
*  l_sel_button-icon_text = 'Vlr. Patrimônio Líquido'.
*  sscrfields-functxt_05  = l_sel_button.
**  End of FF  21.02.2023


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'F00'.
      PERFORM f_verifica_campos.
    WHEN 'F01'.
      CALL TRANSACTION 'ZGL078'. " Cadastro de Empresa
    WHEN 'F02'.
      CALL TRANSACTION 'ZGL079'. " Parâmetro Contabilização
*    WHEN 'F04'.
*      PERFORM check_reflexa_pl.
*      PERFORM fill_shdb USING 'ZGLR075'.
    WHEN 'F03'.
      SUBMIT zregister_data     WITH p_db_tab  = 'ZGLT0111'
                                WITH p_stcnam = 'ZGLT0111_OUT'
                                WITH p_scmant = '0171'
                                WITH p_title = 'Cadastro de Valor de Patrimônio Líquido'
    AND RETURN.
    WHEN 'F04' .
      CALL TRANSACTION 'ZGL084'. " Empresas Exceção Equivalencia Patrimonial
    WHEN 'F05' .
      "CALL TRANSACTION 'ZGL088'.
      submit ZGLR082 AND RETURN.
    WHEN 'BACK'.
      "LEAVE TO SCREEN 0.
      LEAVE PROGRAM.
  ENDCASE.


START-OF-SELECTION.

  PERFORM f_verifica_campos.





*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.
  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR 'TB0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_shdb USING p_program p_dynpro p_dynbegin p_fnam p_fval.

  DATA: lwa_dta   TYPE bdcdata.

  lwa_dta-program   = p_program.
  lwa_dta-dynpro    = p_dynpro.
  lwa_dta-dynbegin  = p_dynbegin.
  lwa_dta-fnam      = p_fnam.
  lwa_dta-fval      = p_fval.
  APPEND lwa_dta TO git_dta.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0366   text
*----------------------------------------------------------------------*
FORM fill_shdb  USING    VALUE(p_0366).

  IF  p_invra IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investidora é Obrigatório!'.
    EXIT.
  ENDIF.
  IF  p_invda IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investida é Obrigatório!'.
    EXIT.
  ENDIF.
  IF  p_monat IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Mês é Obrigatório!'.
    EXIT.
  ENDIF.
  IF p_gjahr IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Ano é Obrigatório!'.
    EXIT.
  ENDIF.

  opt =
  VALUE #(
    dismode = 'E'
    racommit = abap_true
    nobinpt = abap_true
  ).

  IF p_0366 = 'ZGLR074'.
    REFRESH: git_dta, git_msg.
    PERFORM zf_shdb USING: 'ZGLR074' '0100' 'X'  ' '              ' ',
                           ' '       ' '    ' '  'BDC_CURSOR'    'P_ANO',
                           ' '       ' '    ' '  'BDC_OKCODE'    '=SEARCH',
                           ' '       ' '    ' '  'P_BUKRS_INVRA'  p_invra,
                           ' '       ' '    ' '  'P_BUKRS_INVDA'  p_invda,
                           ' '       ' '    ' '  'P_MES'          p_monat,
                           ' '       ' '    ' '  'P_ANO'          p_gjahr.
    CALL TRANSACTION 'ZGL081' USING git_dta OPTIONS FROM opt
        MESSAGES INTO git_msg.
  ELSE.
    REFRESH: git_dta, git_msg.
    PERFORM zf_shdb USING: 'ZGLR075' '0100' 'X'  ' '              ' ',
                           ' '       ' '    ' '  'BDC_CURSOR'    'P_ANO',
                           ' '       ' '    ' '  'BDC_OKCODE'    '=SEARCH',
                           ' '       ' '    ' '  'P_BUKRS_INVRA'  p_invra,
                           ' '       ' '    ' '  'P_BUKRS_INVDA'  p_invda,
                           ' '       ' '    ' '  'P_MES'          p_monat,
                           ' '       ' '    ' '  'P_ANO'          p_gjahr.
    CALL TRANSACTION 'ZGL082' USING git_dta OPTIONS FROM opt
        MESSAGES INTO git_msg.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_REFLEXA_PL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_reflexa_pl .

*  SELECT SINGLE * FROM zglt0104
*      INTO @DATA(ls_104)
*      WHERE investidora = @s_invra-low
*        AND investida   = @s_invda-low.
*
*  IF sy-subrc = 0 AND ls_104-moeda_funcional = 'USD'.
*
**    SELECT SINGLE * FROM zglt0112
**      INTO @DATA(wa_zglt0112)
**      WHERE investidora = @s_invra-low
**        AND investida   = @s_invda-low.
**
**    IF sy-subrc <> 0.
**      CLEAR wa_zglt0112.
**    ENDIF.
**
**    IF wa_zglt0112 IS INITIAL.
**      MESSAGE 'Falta parâmetro de conta para Reflexa do PL' TYPE 'E'." DISPLAY LIKE 'E'.
**    ENDIF.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_verifica_campos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_campos .

  IF  p_invra IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investidora é Obrigatório!'.
    EXIT.
  ENDIF.
  IF  p_invda IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investida é Obrigatório!'.
    EXIT.
  ENDIF.
  IF  p_monat IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Mês é Obrigatório!'.
    EXIT.
  ENDIF.
  IF p_gjahr IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Ano é Obrigatório!'.
    EXIT.
  ENDIF.

  PERFORM f_executa_equi.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_executa_equi
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_executa_equi .

  SUBMIT zglr080 WITH p_monat = p_monat
                 WITH p_exer = p_gjahr
                 WITH p_ivtd = p_invra
                 WITH p_ivda = p_invda
                 AND RETURN.

ENDFORM.
