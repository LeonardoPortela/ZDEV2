*&---------------------------------------------------------------------*
*& Include ZLESR0182_TMP
*&---------------------------------------------------------------------*

GET TIME STAMP              FIELD l_timestamp.

DESCRIBE TABLE t_xlfas LINES l_xlfas.
DESCRIBE TABLE t_xlfb5 LINES l_xlfb5.
DESCRIBE TABLE t_xlfbk LINES l_xlfbk.
DESCRIBE TABLE t_xlfza LINES l_xlfza.
DESCRIBE TABLE t_xlflr LINES l_xlflr.

w_zlest0253_tmp-mandt       = sy-mandt.
w_zlest0253_tmp-chave       = l_timestamp.
w_zlest0253_tmp-acao        = l_acao.
w_zlest0253_tmp-lfa1_lifnr  = i_lfa1-lifnr.
w_zlest0253_tmp-lfb1_lifnr  = i_lfb1-lifnr.
w_zlest0253_tmp-lfb1_bukrs  = i_lfb1-bukrs.
w_zlest0253_tmp-lfm1_lifnr  = i_lfm1-lifnr.
w_zlest0253_tmp-lfm1_ekorg  = i_lfm1-ekorg.
w_zlest0253_tmp-stcd1       = i_lfa1-stcd1.
w_zlest0253_tmp-stcd2       = i_lfa1-stcd2.
w_zlest0253_tmp-t_xlfas     = l_xlfas.
w_zlest0253_tmp-t_xlfas     = l_xlfas.
w_zlest0253_tmp-t_xlfb5     = l_xlfb5.
w_zlest0253_tmp-t_xlfbk     = l_xlfbk.
w_zlest0253_tmp-t_xlfza     = l_xlfza.
w_zlest0253_tmp-t_xlflr     = l_xlflr.
w_zlest0253_tmp-data_reg    = sy-datum.
w_zlest0253_tmp-hora_reg    = sy-uzeit.
w_zlest0253_tmp-user_reg    = sy-uname.

MODIFY zlest0253_tmp     FROM w_zlest0253_tmp.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
