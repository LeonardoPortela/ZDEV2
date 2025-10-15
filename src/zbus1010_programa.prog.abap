*****           Implementation of object type ZBUS1010             *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
* ---> S4 Migration - 18/07/2023 - CA
    customer          LIKE ukmbp_cms-partner,
    creditcontrolarea LIKE ukm_kkber2sgm-kkber,
*      CUSTOMER LIKE KNKK-KUNNR,
*      CREDITCONTROLAREA LIKE KNKK-KKBER,
* <--- S4 Migration - 18/07/2023 - CA
  END OF key.
end_data object. " Do not change.. DATA is generated

DATA:
* ---> S4 Migration - 18/07/2023 - CA
  klimk_new      TYPE klimk,
  klimk_old      TYPE klimk,
*  klimk_new      TYPE knkk-klimk,
*  klimk_old      TYPE knkk-klimk,
* <--- S4 Migration - 18/07/2023 - CA
  waers          TYPE t014-waers,
  v_erro         TYPE cmp_error,
  v_nivel        TYPE sy-tabix,
  v_total_niveis TYPE syst-tabix,
  mail_aprovador TYPE bapiaddr3-e_mail,
  v_aprovador    TYPE zfit_aprovadores-aprovador,
* ---> S4 Migration - 18/07/2023 - CA
  kunnr          TYPE ukmbp_cms-partner,
*  kunnr          TYPE knkk-kunnr,
* <--- S4 Migration - 18/07/2023 - CA
  name1          TYPE kna1-name1,
  v_mode         TYPE c,
  v_user_email   TYPE bapibname-bapibname,
  name_last      TYPE addr3_data-name_last,
  name_first     TYPE addr3_data-name_first,
  wf_initiator   TYPE wfsyst-initiator.
DATA:
  t_aprovadores       TYPE TABLE OF zfit_aprovadores,
  w_aprovadores       TYPE zfit_aprovadores,
  w_addr              TYPE bapiaddr3,
  t_return            TYPE TABLE OF bapiret2,
  w_dados_aprovadores TYPE zfiw_dados_aprovadores,
  t_dados_aprovadores TYPE TABLE OF zfiw_dados_aprovadores.

DATA: t_bdc  LIKE bdcdata OCCURS 0 WITH HEADER LINE,
      t_mess LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.


begin_method zbuscar_aprovadores changing container.
DATA: usuario        TYPE adrp-name_first,
      pnum           TYPE usr21-persnumber,
      mail_initiator TYPE bapiaddr3-e_mail.

swc_get_element container 'KLIMK_NEW' klimk_new.
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

CHECK NOT object-key-creditcontrolarea IS INITIAL.

*   Selecionar Nome do Cliente.
SELECT SINGLE name1
  FROM kna1
  INTO name1
 WHERE kunnr = object-key-customer.

*   Selecionar Moeda.
SELECT SINGLE waers
  FROM t014
  INTO waers
 WHERE kkber = object-key-creditcontrolarea.

IF sy-subrc IS INITIAL.
  CLEAR: v_erro.
  REFRESH t_aprovadores.
*   Buscar aprovadores
  CALL FUNCTION 'Z_FI_WF_BUSCAR_APROVADORES'
    EXPORTING
      kkber         = object-key-creditcontrolarea
      waers         = waers
      klimk         = klimk_new
    IMPORTING
      v_erro        = v_erro
    TABLES
      t_aprovadores = t_aprovadores
    EXCEPTIONS
      not_found     = 1
      OTHERS        = 2.

  IF NOT sy-subrc IS INITIAL OR v_erro = 'X'.
*     Se der erro na função, move usuário administrador do WF fixo,
* senão, não encontrou os aprovadores e foi retornado o aprovador adm.
    IF v_erro IS INITIAL.
      v_aprovador = 'USABAP'.
      v_user_email = v_aprovador.
      v_erro = 'X'.                      " Aprovadores não encontrados
    ELSE.
      READ TABLE t_aprovadores INTO w_aprovadores INDEX 1.
      v_user_email = w_aprovadores-aprovador.
      v_aprovador = w_aprovadores-aprovador.
    ENDIF.

    CLEAR: w_addr.
*     Buscar Email do Usuário Adm.do WF
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
*     Se a função executar com sucesso e aprovadores forem encontrados.
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
*       Mover dados do aprovador para a tabela de dados do aprov.
      MOVE:w_aprovadores-aprovador TO w_dados_aprovadores-aprovador,
           w_addr-e_mail TO w_dados_aprovadores-email.

*       Selecionar o nome completo do aprovador.
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
ENDIF.

* Buscar número de níveis de aprovação.
DESCRIBE TABLE t_aprovadores LINES v_total_niveis.
SORT t_aprovadores BY limite_de ASCENDING.

swc_set_table container 'T_DADOS_APROVADORES' t_dados_aprovadores.
swc_set_table container 'T_APROVADORES' t_aprovadores.
swc_set_element container 'V_TOTAL_NIVEIS' v_total_niveis.
swc_set_element container 'V_ERRO' v_erro.
swc_set_element container 'MAIL_APROVADOR' mail_aprovador.
swc_set_element container 'WAERS' waers.
swc_set_element container 'NAME1' name1.
end_method.

begin_method zler_aprovador_corrente changing container.

swc_get_table container 'T_APROVADORES' t_aprovadores.
swc_get_table container 'T_DADOS_APROVADORES' t_dados_aprovadores.
swc_get_element container 'V_NIVEL' v_nivel.

SORT t_aprovadores BY limite_de ASCENDING.

* Verifica se nível corrente foi importado.
CHECK NOT v_nivel IS INITIAL.
*   Ler aprovador corrente.
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
      texto_container TYPE zwf_texto_rejeicao OCCURS 0.

CALL FUNCTION 'Z_WF_TELA_MOTIVO_REJEICAO'
  EXPORTING
    kkber           = object-key-creditcontrolarea
    kunnr           = object-key-customer
  TABLES
    texto_container = texto_container.

swc_set_table container 'TEXTO_CONTAINER' texto_container.
end_method.


begin_method zdesbloqueio_limite changing container.

PERFORM zf_insere_linha USING:
'X'    'SAPMF02C'     '0100',         " Primeira tela
' '    'BDC_CURSOR'    'RF02L-D0210',
' '    'RF02L-KUNNR'   object-key-customer,
' '    'RF02L-KKBER'   object-key-creditcontrolarea,
' '    'RF02L-D0210'   'X',
' '    'BDC_OKCODE'    '/00'.

PERFORM zf_insere_linha USING:
'X'     'SAPMF02C'     '0210',            "Segunda Tela
' '     'BDC_CURSOR'    'KNKK-CRBLB',
' '     'KNKK-CRBLB'    space,
' '     'BDC_OKCODE'    '=UPDA'.
v_mode = 'N'.
"*---> 12/07/2023 - Migração S4
*    call transaction 'FD32' using t_bdc mode v_mode update 'S'
*                                       messages into t_mess.
CALL FUNCTION 'ZFD32_TO_UKM_BP'
  EXPORTING
    it_bdcdata = t_bdc
  IMPORTING
    it_messtab = t_mess.
"*<--- 12/07/2023 - Migração S4
end_method.

*&---------------------------------------------------------------------*
*&      Form  zf_insere_linha
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0957   text
*      -->P_0958   text
*      -->P_0959   text
*----------------------------------------------------------------------*
FORM zf_insere_linha USING p_start TYPE c p_name TYPE c p_value.
  CLEAR t_bdc.
  MOVE p_start TO t_bdc-dynbegin.
  IF p_start EQ 'X'.
    MOVE:
      p_name  TO t_bdc-program,
      p_value TO t_bdc-dynpro.
  ELSE.
    MOVE:
      p_name  TO t_bdc-fnam,
      p_value TO t_bdc-fval.
  ENDIF.
  APPEND t_bdc.
ENDFORM.                    " zf_insere_linha

begin_method zdisplay changing container.

* ---> S4 Migration - 19/07/2023 - DG
*REFRESH t_bdc.
*PERFORM zf_insere_linha USING:
*'X'    'SAPMF02C'     '0100',         " Primeira tela
*' '    'BDC_CURSOR'    'RF02L-D0210',
*' '    'RF02L-KUNNR'   object-key-customer,
*' '    'RF02L-KKBER'   object-key-creditcontrolarea,
*' '    'RF02L-D0210'   'X',
*' '    'BDC_OKCODE'    '/00'.
*
*v_mode = 'E'.
*
*
*CALL TRANSACTION 'FD33' USING t_bdc MODE v_mode.


  SET PARAMETER ID 'UKM_SEGMENT' FIELD  object-key-creditcontrolarea.

  DATA(lo_request) = NEW cl_bupa_navigation_request( ).

  lo_request->set_partner_number( object-key-customer ).

  lo_request->set_bupa_activity( '03' ). " 01 - Create, 02 - Change, 03 - Display

  lo_request->set_maintenance_id( 'B' ). " B - Partner

  DATA(lo_options) = NEW cl_bupa_dialog_joel_options( ).

  lo_options->set_locator_visible( space ).

  CALL METHOD cl_bupa_dialog_joel=>start_with_navigation
    EXPORTING
      iv_request              = lo_request
      iv_options              = lo_options
      iv_in_new_internal_mode = abap_true
    EXCEPTIONS
      already_started         = 1
      not_allowed             = 2
      OTHERS                  = 3.

* <--- S4 Migration - 19/07/2023 - DG3 - DG

end_method.

begin_method zapagar_comentario changing container.

DATA: gn_fname    TYPE thead-tdname,
      gn_fid      TYPE  thead-tdid,
      gn_language TYPE thead-tdspras.

CONCATENATE 'ZFI_ADM_LIM_CRED' object-key-creditcontrolarea
                               object-key-customer
                               INTO gn_fname.

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
*   TEXTMEMORY_ONLY = ' '
*   LOCAL_CAT       = ' '
  EXCEPTIONS
    not_found       = 1
    OTHERS          = 2.

IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


end_method.
