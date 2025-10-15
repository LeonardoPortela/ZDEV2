*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 16/09/2025                                              &*
*& Descrição: Cockpit Parametrização Reforma Tributária               &*
*& Transação: ZTAXREFORM (Parametrização Reforma Tributária)          &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2SPO |16/09/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
REPORT zfir_tax_reform.

TABLES: sscrfields.

*--------------------------------------------------------------------*
* V A R I A B L E S                                                  *
*--------------------------------------------------------------------*
DATA: gv_db_tab TYPE tabname,
      gv_stcnam TYPE tabname,
      gv_scmant TYPE c LENGTH 4,
      gv_title  TYPE cua_tit_tx,
      gv_act_01 TYPE cua_tit_tx.

*--------------------------------------------------------------------*
* C O N S T A N T S                                                  *
*--------------------------------------------------------------------*
CONSTANTS: gc_db_tab1 TYPE tabname    VALUE 'ZNCMAGROP',
           gc_db_tab2 TYPE tabname    VALUE 'ZFOPEXAGROP',
           gc_stcnam1 TYPE tabname    VALUE 'ZNCMAGROP_OUT',
           gc_stcnam2 TYPE tabname    VALUE 'ZFOPEXAGROP_OUT',
           gc_scmant1 TYPE c LENGTH 4 VALUE '0327',
           gc_scmant2 TYPE c LENGTH 4 VALUE '0328'.

*--------------------------------------------------------------------*
* S E L E C T I O N - S C R E E N                                    *
*--------------------------------------------------------------------*
**********************************************************************
* Tela de Sele  o Principal
**********************************************************************
* Tela de seleção
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN: SKIP,
  PUSHBUTTON /1(75) bt_man01 USER-COMMAND btm01, "Manutenção de NCM obrigatórios
  SKIP,
  PUSHBUTTON /1(75) bt_man02 USER-COMMAND btm02. "Manutenção de CFOP de exceção

SELECTION-SCREEN END OF BLOCK b01.

*--------------------------------------------------------------------*
* I N I T I A L I Z A T I O N                                        *
*--------------------------------------------------------------------*
INITIALIZATION.
* Configuração Tela Report com botão.
  PERFORM zf_setting_screen.
*--------------------------------------------------------------------*
* A T - S E L E C T I O N  S C R E E N                               *
*--------------------------------------------------------------------*
AT SELECTION-SCREEN.
* Chama a manutenção de cadastro de parâmetros.
  PERFORM zf_chama_manutencao_tabl.
*--------------------------------------------------------------------*
* S U B R O U T I N E S                                              *
*--------------------------------------------------------------------*
*&-------------------------------------------------------------------*
*&      Form  ZF_SETTING_SCREEN
*&-------------------------------------------------------------------*
*       Configuração Tela Report com botão
*--------------------------------------------------------------------*
FORM zf_setting_screen.

  bt_man01 = TEXT-002. "Manutenção de NCM obrigatórios
  bt_man02 = TEXT-003. "Manutenção de de CFOP de exceção

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_CHAMA_MANUTENCAO_TABL
*&---------------------------------------------------------------------*
*& Chama a manutenção de cadastro de parâmetros
*&---------------------------------------------------------------------*
FORM zf_chama_manutencao_tabl.

  CASE sscrfields-ucomm.
    WHEN 'BTM01'. "Manutenção de NCM obrigatórios
      gv_db_tab = gc_db_tab1.
      gv_stcnam = gc_stcnam1.
      gv_scmant = gc_scmant1.
      gv_title  = TEXT-002. "Manutenção de NCM obrigatórios

    WHEN 'BTM02'. "Manutenção de CFOP de exceção
      gv_db_tab = gc_db_tab2.
      gv_stcnam = gc_stcnam2.
      gv_scmant = gc_scmant2.
      gv_title  = TEXT-003. "Manutenção de CFOP de exceção

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

  gv_act_01 = TEXT-004. "Log Modificação
* Executa o programa de Cadastro de Dados.
  SUBMIT zregister_data WITH p_db_tab = gv_db_tab
                        WITH p_stcnam = gv_stcnam
                        WITH p_scmant = gv_scmant
                        WITH p_title  = gv_title
                        WITH p_act_01 = gv_act_01
                    AND RETURN.

  CLEAR: gv_db_tab, gv_stcnam, gv_scmant, gv_title, gv_act_01.

ENDFORM.
