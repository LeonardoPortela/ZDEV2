"Name: \PR:SAPLJ_1B_NFE\FO:CALL_MESSAGE_SYSTEM_COMM\SE:BEGIN\EI
ENHANCEMENT 0 Z_VALIDA_INFO_XML_SAP.
*

*TYPE 1 Tipo  BAPI_MTYPE  CHAR  1 0 Ctg.mens.: S sucesso, E erro, W aviso, I inform., A cancel.
*ID 1 Tipo  SYMSGID CHAR  20  0 Classe de mensagem
*NUMBER 1 Tipo  SYMSGNO NUMC  3 0 Nº mensagem
*MESSAGE  1 Tipo  BAPI_MSG  CHAR  220 0 Texto de mensagem
*LOG_NO 1 Tipo  BALOGNR CHAR  20  0 Log de aplicação: nº de log
*LOG_MSG_NO 1 Tipo  BALMNR  NUMC  6 0 Log de aplicação: nº sequencial interno da mensagem
*MESSAGE_V1 1 Tipo  SYMSGV  CHAR  50  0 Variável mensagens
*MESSAGE_V2 1 Tipo  SYMSGV  CHAR  50  0 Variável mensagens
*MESSAGE_V3 1 Tipo  SYMSGV  CHAR  50  0 Variável mensagens
*MESSAGE_V4 1 Tipo  SYMSGV  CHAR  50  0 Variável mensagens
*PARAMETER  1 Tipo  BAPI_PARAM  CHAR  32  0 Nome de parâmetro
*ROW  1 Tipo  BAPI_LINE INT4  10  0 Linha em parâmetro
*FIELD  1 Tipo  BAPI_FLD  CHAR  30  0 Campo em parâmetro
*SYSTEM 1 Tipo  BAPILOGSYS  CHAR  10  0 Sistema (sistema lógico), do qual provém a mensagem

*006  Inf. Adicionais de Interesse do Fisco excedeu 2000 caracteres.
*007  Inf. Complementares de interesse do Contribuinte excedeu 5000 caracteres.

*  if zcl_string=>LENGTH( CONV #( xmlh-infadfisco ) ) gt 2000.
*    MESSAGE id 'ZJ1B_NFE' TYPE 'S' NUMBER 006.
*    append VALUE #( TYPE = 'E' ID = SY-MSGID NUMBER = SY-MSGNO
*                    MESSAGE_V1 = SY-MSGV1
*                    MESSAGE_V2 = SY-MSGV2
*                    MESSAGE_V3 = SY-MSGV3
*                    MESSAGE_V4 = SY-MSGV4 ) to ut_bapiret2.
*    uv_rfcerror = 1.
*    uv_error = 'V'.
*    exit.
*  endif.
*
*  if zcl_string=>LENGTH( CONV #( xmlh-infadfisco_v2 ) ) gt 2000.
*    MESSAGE id 'ZJ1B_NFE' TYPE 'S' NUMBER 006.
*    append VALUE #( TYPE = 'E' ID = SY-MSGID NUMBER = SY-MSGNO
*                    MESSAGE_V1 = SY-MSGV1
*                    MESSAGE_V2 = SY-MSGV2
*                    MESSAGE_V3 = SY-MSGV3
*                    MESSAGE_V4 = SY-MSGV4 ) to ut_bapiret2.
*    uv_error = 'V'.
*    exit.
*  endif.
*
*  if zcl_string=>LENGTH( CONV #( xmlh-infcomp ) ) gt 5000.
*    MESSAGE id 'ZJ1B_NFE' TYPE 'S' NUMBER 007.
*    append VALUE #( TYPE = 'E' ID = SY-MSGID NUMBER = SY-MSGNO
*                    MESSAGE_V1 = SY-MSGV1
*                    MESSAGE_V2 = SY-MSGV2
*                    MESSAGE_V3 = SY-MSGV3
*                    MESSAGE_V4 = SY-MSGV4 ) to ut_bapiret2.
*    uv_error = 'V'.
*    uv_rfcerror = 1.
*    exit.
*  endif.

ENDENHANCEMENT.
