*****           Implementation of object type ZBUS2032             *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    salesdocument LIKE vbak-vbeln,
  END OF key.
end_data object. " Do not change.. DATA is generated

DATA: v_lines        TYPE i,
      v_total_niveis TYPE syst-tabix,
      valor1         TYPE vbak-netwr,
      mail_aprovador TYPE ad_smtpadr,
      l_addr         TYPE bapiaddr3,
      name1          TYPE kna1-name1,
      v_mode         TYPE c,
      v_user_email   TYPE bapibname-bapibname,
      name_last      TYPE addr3_data-name_last,
      name_first     TYPE addr3_data-name_first,
      wf_initiator   TYPE wfsyst-initiator,
      kwmeng         TYPE vbap-kwmeng,
      klimk          TYPE knkk-klimk, "#EC CI_USAGE_OK[2227014]
      butxt          TYPE t001-butxt,
      v_aprovador    TYPE zsdt_aprovadores-aprovador,
      v_erro         TYPE cmp_error.


DATA: t_aprovadores       TYPE TABLE OF zsdt_aprovadores,
      w_aprovadores       TYPE zsdt_aprovadores,
      t_addr              TYPE TABLE OF bapiaddr3,
      w_vbak              TYPE vbak,
      w_addr              TYPE bapiaddr3,
      t_return            TYPE TABLE OF bapiret2,
      w_dados_aprovadores TYPE zfiw_dados_aprovadores,
      t_dados_aprovadores TYPE TABLE OF zfiw_dados_aprovadores,
      t_bdc               LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      t_mess              LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

* ---> S4 Migration - 18/07/2023 - CA
DATA: lt_knkk       TYPE STANDARD TABLE OF knkk, "#EC CI_USAGE_OK[2227014]
      lt_data_where TYPE STANDARD TABLE OF zknkk_key,
      wa_data_where TYPE zknkk_key.
* <--- S4 Migration - 18/07/2023 - CA

begin_method zbuscar_aprovadores changing container.
DATA: usuario        TYPE adrp-name_first,
      pnum           TYPE usr21-persnumber,
      mail_initiator TYPE bapiaddr3-e_mail.

swc_get_element container 'VALOR1' valor1.
swc_get_element container 'WF_INITIATOR' wf_initiator.

* Buscar o Email do solicitante do WF
CLEAR: v_user_email, w_addr.
v_user_email = wf_initiator+02(10).

CALL FUNCTION 'BAPI_USER_GET_DETAIL'
  EXPORTING
    username = v_user_email
  IMPORTING
    address  = w_addr
  TABLES
    return   = t_return.

IF NOT w_addr-e_mail IS INITIAL.
  MOVE w_addr-e_mail TO mail_initiator.
  swc_set_element container 'MAIL_INITIATOR' mail_initiator.
ENDIF.

CHECK NOT object-key-salesdocument IS INITIAL.

*   Buscar dados do documento de vendas.
SELECT SINGLE *
  FROM vbak
  INTO w_vbak
 WHERE vbeln = object-key-salesdocument.
CHECK sy-subrc IS INITIAL.

*   Buscar qtde no item do documento de vendas.
SELECT SINGLE kwmeng
  FROM vbap
  INTO kwmeng
 WHERE vbeln = object-key-salesdocument.

*   Selecionar Limite de Crédito.
* ---> S4 Migration - 18/07/2023 - CA
*    SELECT SINGLE KLIMK
*      FROM KNKK
*      INTO KLIMK
*     WHERE KUNNR = W_VBAK-KUNNR AND
*           KKBER = W_VBAK-KKBER.

wa_data_where-kunnr = w_vbak-kunnr.
wa_data_where-kkber = w_vbak-kkber.
APPEND wa_data_where TO lt_data_where.


CALL FUNCTION 'Z_FROM_TO_KNKK'
  TABLES
    t_data_where = lt_data_where
    t_knkk       = lt_knkk.


IF lt_knkk[] IS NOT INITIAL.
  READ TABLE lt_knkk INTO DATA(ls_knkk) INDEX 1.
  IF sy-subrc = 0.
    klimk = ls_knkk-klimk.
  ENDIF.
ENDIF.
* <--- S4 Migration - 18/07/2023 - CA

*   Selecionar Nome do Cliente.
SELECT SINGLE name1
  FROM kna1
  INTO name1
 WHERE kunnr = w_vbak-kunnr.

*   Selecionar Nome da Empresa.
SELECT SINGLE butxt
  FROM t001
  INTO butxt
 WHERE bukrs = w_vbak-bukrs_vf.

CALL FUNCTION 'Z_SD_WF_BUSCAR_APROVADORES'
  EXPORTING
    vkorg                    = w_vbak-vkorg
    klimk                    = klimk
  IMPORTING
    v_erro                   = v_erro
  TABLES
    t_aprovadores            = t_aprovadores
  EXCEPTIONS
    aprovador_nao_encontrado = 1
    OTHERS                   = 2.

IF NOT sy-subrc IS INITIAL OR v_erro = 'X'.
*    Se der erro na função, move usuário administrador do WF fixo,
*  senão, não encontrou os aprovadores e foi retornado o aprovador adm.
  IF v_erro IS INITIAL.
    v_aprovador = 'USABAP'.
    v_user_email = v_aprovador.
    v_erro = 'X'.                     " Aprovadores não encontrados
  ELSE.
    READ TABLE t_aprovadores INTO w_aprovadores INDEX 1.
    v_user_email = w_aprovadores-aprovador.
    v_aprovador = w_aprovadores-aprovador.
  ENDIF.

  CLEAR: w_addr.
*    Buscar Email do Usuário Adm.do WF
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username = v_user_email
    IMPORTING
      address  = w_addr
    TABLES
      return   = t_return.

  IF NOT w_addr-e_mail IS INITIAL.
    MOVE w_addr-e_mail TO mail_aprovador.
  ENDIF.
  swc_set_element container 'V_APROVADOR' v_aprovador.
  swc_set_element container 'MAIL_APROVADOR' mail_aprovador.
ELSE.
*    Se a função executar com sucesso e aprovadores forem encontrados
  LOOP AT t_aprovadores INTO w_aprovadores.

    CLEAR: w_addr, t_return, w_dados_aprovadores, v_user_email.
    v_user_email = w_aprovadores-aprovador.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = v_user_email
      IMPORTING
        address  = w_addr
      TABLES
        return   = t_return.
*     Mover dados do aprovador para a tabela de dados do aprov.
    MOVE:w_aprovadores-aprovador TO w_dados_aprovadores-aprovador,
         w_addr-e_mail TO w_dados_aprovadores-email.

*     Selecionar o nome completo do aprovador.
    usuario = w_aprovadores-aprovador.
    SELECT SINGLE persnumber
      FROM usr21
      INTO pnum
     WHERE bname = usuario.

    IF sy-subrc IS INITIAL.
      SELECT name_first name_last
        UP TO 1 ROWS
        FROM adrp
        INTO (name_first, name_last)
       WHERE persnumber = pnum.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        MOVE: name_first TO w_dados_aprovadores-name_first,
              name_last TO w_dados_aprovadores-name_last.
        APPEND w_dados_aprovadores TO t_dados_aprovadores.
        CLEAR w_dados_aprovadores.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDIF.

DESCRIBE TABLE t_aprovadores LINES v_total_niveis.
SORT t_aprovadores BY valor_de ASCENDING.

swc_set_table container 'T_DADOS_APROVADORES' t_dados_aprovadores.
swc_set_table container 'T_APROVADORES' t_aprovadores.
swc_set_element container 'V_TOTAL_NIVEIS' v_total_niveis.
swc_set_element container 'V_ERRO' v_erro.
swc_set_element container 'MAIL_APROVADOR' mail_aprovador.
swc_set_element container 'WAERK' w_vbak-waerk.
swc_set_element container 'NAME1' name1.
swc_set_element container 'BUTXT' butxt.
swc_set_element container 'KLIMK' klimk.
swc_set_element container 'KWMENG' kwmeng.
swc_set_element container 'NETWR' w_vbak-netwr.
end_method.

begin_method zler_aprovador_corrente changing container.
DATA:
  v_nivel     TYPE syst-tabix,
  v_aprovador TYPE wfsyst-agent.
swc_get_table container 'T_APROVADORES' t_aprovadores.
swc_get_table container 'T_DADOS_APROVADORES' t_dados_aprovadores.
swc_get_element container 'V_NIVEL' v_nivel.
SORT t_aprovadores BY valor_de ASCENDING.

CHECK NOT v_nivel IS INITIAL.

READ TABLE t_aprovadores INTO w_aprovadores INDEX v_nivel.
CHECK sy-subrc IS INITIAL.
READ TABLE t_dados_aprovadores INTO w_dados_aprovadores
                     WITH KEY aprovador = w_aprovadores-aprovador.
MOVE: w_dados_aprovadores-name_first TO name_first,
      w_dados_aprovadores-name_last TO name_last,
      w_dados_aprovadores-email TO mail_aprovador.

CONCATENATE 'US' w_aprovadores-aprovador INTO v_aprovador.

swc_set_element container 'V_APROVADOR' v_aprovador.
swc_set_element container 'MAIL_APROVADOR' mail_aprovador.
swc_set_element container 'NAME_FIRST' name_first.
swc_set_element container 'NAME_LAST' name_last.

end_method.

begin_method zcomentario changing container.
DATA:
      texto_container LIKE zwf_texto_rejeicao OCCURS 0.

CALL FUNCTION 'Z_WF_SD_TELA_MOTIVO_REJEICAO'
  EXPORTING
    vbeln           = object-key-salesdocument
  TABLES
    texto_container = texto_container.

swc_set_table container 'TEXTO_CONTAINER' texto_container.
end_method.

begin_method zapagar_comentario changing container.
DATA: gn_fname    TYPE thead-tdname,
      gn_fid      TYPE  thead-tdid,
      gn_language TYPE thead-tdspras.

CONCATENATE 'ZSD_LIM_CRED' object-key-salesdocument INTO gn_fname.

gn_fid = 'ST'.
gn_language = 'PT'.

* Apagar texto do comentário para que um próximo WF não o carregue.
CALL FUNCTION 'DELETE_TEXT'
  EXPORTING
    id              = gn_fid
    language        = gn_language
    name            = gn_fname
    object          = 'TEXT'
    savemode_direct = 'X'
  EXCEPTIONS
    not_found       = 1
    OTHERS          = 2.

end_method.

begin_method zcomentario_aprov changing container.
DATA:
      texto_container_ap LIKE zwf_texto_rejeicao OCCURS 0.

CALL FUNCTION 'Z_WF_SD_TELA_MOTIVO_APROVACAO'
  EXPORTING
    vbeln              = object-key-salesdocument
  TABLES
    texto_container_ap = texto_container_ap.

swc_set_table container 'TEXTO_CONTAINER_AP' texto_container_ap.
end_method.

begin_method zapagar_comentario_aprovacao changing container.

DATA: gn_fname    TYPE thead-tdname,
      gn_fid      TYPE  thead-tdid,
      gn_language TYPE thead-tdspras.

CONCATENATE 'ZSD_LIM_CRED_AP' object-key-salesdocument INTO gn_fname.

gn_fid = 'ST'.
gn_language = 'PT'.

* Apagar texto do comentário para que um próximo WF não o carregue.
CALL FUNCTION 'DELETE_TEXT'
  EXPORTING
    id              = gn_fid
    language        = gn_language
    name            = gn_fname
    object          = 'TEXT'
    savemode_direct = 'X'
  EXCEPTIONS
    not_found       = 1
    OTHERS          = 2.


end_method.

begin_method zdesbloqueio_remessa changing container.
DATA:
  orderheaderin    LIKE bapisdh1,
  orderheaderinx   LIKE bapisdh1x,
  return           LIKE bapiret2 OCCURS 0,
  ret2             TYPE bapiret2,
  salesdocument    TYPE bapivbeln-vbeln,
  message          TYPE bapi_msg,
  erro_desbloqueio TYPE char1,
  w_return         TYPE bapiret2.

swc_get_element container 'OrderHeaderIn' orderheaderin.
swc_get_element container 'OrderHeaderInx' orderheaderinx.

orderheaderinx-dlv_block = 'X'.
orderheaderinx-updateflag = 'U'.
orderheaderin-dlv_block = space.

salesdocument = object-key-salesdocument.
CALL FUNCTION 'Z_WF_SD_DESBLOQUEIO_REMESSA'
  EXPORTING
    salesdocument  = salesdocument
    orderheaderin  = orderheaderin
    orderheaderinx = orderheaderinx
  TABLES
    return         = return.

CLEAR: message, erro_desbloqueio.
* Verificar se houveram erros.
READ TABLE return INTO w_return WITH KEY type = 'E'.
IF sy-subrc IS INITIAL.
*   Se a O.V está em processamento/bloqueada.
  IF w_return-id EQ 'V1' AND
     w_return-number EQ '042'.
* Se algum usuário estiver bloqueando a O.V, uma exceção é disparada e
* a tarefa é reprocessada automaticamente via job standard.
    exit_return 9999 w_return-message_v1 w_return-message_v2
    w_return-message_v3 w_return-message_v4.
  ELSE.
    message = w_return-message.
    erro_desbloqueio = 'X'.
    swc_set_element container 'MESSAGE' message.
    swc_set_element container 'ERRO_DESBLOQUEIO' erro_desbloqueio.
  ENDIF.
ELSE.
  COMMIT WORK AND WAIT.
ENDIF.

swc_set_table container 'RETURN' return.
end_method.













