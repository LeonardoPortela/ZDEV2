FUNCTION zsdmf_popup_radio_options.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_QUESTION) TYPE  MSGXX
*"     REFERENCE(IV_TEXT01) TYPE  MSGXX
*"     REFERENCE(IV_TEXT02) TYPE  MSGXX
*"  EXPORTING
*"     REFERENCE(EV_ANSWER) TYPE  CHAR1
*"----------------------------------------------------------------------
  "K_KKB_POPUP_RADIO3
  CLEAR zsde0035.

  zsde0035-text = iv_question.
  zsde0035-opt1_txt = iv_text01.
  zsde0035-opt2_txt = iv_text02.

  CALL SCREEN 9001 STARTING AT 35 05.

  ev_answer = zsde0035-answer.

ENDFUNCTION.
