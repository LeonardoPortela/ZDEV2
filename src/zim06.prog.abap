*&---------------------------------------------------------------------*
*& Report  ZIM06
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zim06.

TABLES: csks, zim01_sol_ap_inv.

DATA: w_ukurs_usd TYPE tcurr-ukurs,
      w_ukurs_eur TYPE tcurr-ukurs.

DATA: BEGIN OF t_inv OCCURS 0.
        INCLUDE STRUCTURE zim01_sol_ap_inv.
DATA: END OF t_inv.

DATA: BEGIN OF t_ctl OCCURS 0.
        INCLUDE STRUCTURE zim02_sol_ap_ctl.
DATA: END OF t_ctl.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_kostl  FOR csks-kostl  MATCHCODE OBJECT kost,
                so_ano    FOR zim01_sol_ap_inv-ano NO INTERVALS NO-EXTENSION OBLIGATORY,
                s_safra   FOR zim01_sol_ap_inv-safra NO INTERVALS NO-EXTENSION,
                s_safra2  FOR zim01_sol_ap_inv-safra2 NO INTERVALS NO-EXTENSION.
PARAMETERS: p_data2 TYPE sy-datum OBLIGATORY MATCHCODE OBJECT zimdt_cambio,"ze_data,
            p_data  TYPE tcurr-gdatu NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  PERFORM f_busca_cambio.

  PERFORM f_busca_dados.

  PERFORM f_modifca_dados.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_CAMBIO
*&---------------------------------------------------------------------*
FORM f_busca_cambio .


  DATA: w_val(10).

  w_val = p_data2.
  CLEAR p_data2.
  CONCATENATE w_val+6(2) w_val+4(2) w_val(4) INTO p_data2.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = p_data2
    IMPORTING
      output = p_data.


  p_data2 = w_val.

  SELECT SINGLE ukurs FROM tcurr
    INTO w_ukurs_usd
    WHERE kurst = 'PLAN' AND
          fcurr = 'BRL'  AND
          tcurr = 'USD'  AND
          gdatu = p_data.

  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Não existe tx cambio USD para data informada'.
  ENDIF.

  SELECT SINGLE ukurs FROM tcurr
    INTO w_ukurs_eur
    WHERE kurst = 'PLAN' AND
          fcurr = 'BRL'  AND
          tcurr = 'EUR'  AND
          gdatu = p_data.
  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Não existe tx cambio EUR para data informada'.
  ENDIF.

*  IF w_ukurs_eur IS INITIAL OR
*     w_ukurs_eur IS INITIAL.
*    MESSAGE i000(z01) WITH 'Processo encerrado'.
*    STOP.
*  ENDIF.

ENDFORM.                    " F_BUSCA_CAMBIO
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
FORM f_busca_dados .

  SELECT * FROM zim02_sol_ap_ctl INTO TABLE t_ctl
  WHERE kostl IN so_kostl AND
        ano   IN so_ano   AND
        safra IN s_safra  AND
        safra2 IN s_safra2.

  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Dados não encontrados na tabela ZIM02_SOL_AP_CTL'.
    MESSAGE i000(z01) WITH 'Processo encerrado'.
    STOP.
  ENDIF.

  SELECT * FROM zim01_sol_ap_inv INTO TABLE t_inv
    WHERE kostl IN so_kostl AND
          ano   IN so_ano   AND
          safra IN s_safra  AND
          safra2 IN s_safra2.

  IF sy-subrc <> 0.
    MESSAGE i000(z01) WITH 'Dados não encontrados na tabela ZIM01_SOL_AP_INV'.
*    MESSAGE i000(z01) WITH 'Processo encerrado'.
*    STOP.
  ENDIF.

ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_MODIFCA_DADOS
*&---------------------------------------------------------------------*
FORM f_modifca_dados .

  DATA: v_answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Atualizar'
      text_question         = 'Deseja a taxa de câmbio dos planejados?'
      text_button_1         = 'Sim'
      icon_button_1         = 'ICON_OKAY'
      text_button_2         = 'Não'
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = v_answer.
  IF v_answer NE '1'.
    STOP.
  ELSE.

    LOOP AT t_ctl.
      t_ctl-tx_usd = w_ukurs_usd.
      t_ctl-tx_eur = w_ukurs_eur.
      MODIFY t_ctl.
    ENDLOOP.


    LOOP AT t_inv.
      t_inv-tx_usd = w_ukurs_usd.
      t_inv-tx_eur = w_ukurs_eur.
      IF NOT w_ukurs_usd IS INITIAL.
        t_inv-vl_usd = t_inv-vlr_total / w_ukurs_usd.
      ELSE.
        t_inv-vl_usd = 0.
      ENDIF.
      IF NOT w_ukurs_eur IS INITIAL.
        t_inv-vl_eur = t_inv-vlr_total / w_ukurs_eur.
      ELSE.
        t_inv-vl_eur = 0.
      ENDIF.
      MODIFY t_inv.
    ENDLOOP.

    MODIFY zim02_sol_ap_ctl FROM TABLE t_ctl.
    MODIFY zim01_sol_ap_inv FROM TABLE t_inv.

    COMMIT WORK AND WAIT.

    MESSAGE i000(z01) WITH 'Dados atualizados com sucesso!'.
  ENDIF.
ENDFORM.                    " F_MODIFCA_DADOS
