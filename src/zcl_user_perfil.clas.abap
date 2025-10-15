class ZCL_USER_PERFIL definition
  public
  final
  create public .

public section.

  interfaces ZIF_USER_PERFIL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_USER_PERFIL IMPLEMENTATION.


  METHOD ZIF_USER_PERFIL~GERA_ERRO_GERAL.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_USER_PERFIL
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_USER_PERFIL=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_USER_PERFIL=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_USER_PERFIL=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_USER_PERFIL=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_USER_PERFIL~GET_INSTANCE.

    IF ZIF_USER_PERFIL~AT_USER_PERFIL IS NOT BOUND.
      CREATE OBJECT ZIF_USER_PERFIL~AT_USER_PERFIL TYPE ZCL_USER_PERFIL.
    ENDIF.

    R_USER_PERFIL = ZIF_USER_PERFIL~AT_USER_PERFIL.

  ENDMETHOD.


  METHOD ZIF_USER_PERFIL~SET_ATRIBUIR_ATRIBUTO.



  ENDMETHOD.


  METHOD zif_user_perfil~set_atribuir_funcoes.

    DATA: lc_activitygroups	TYPE rssbr_t_badi_bapiagr,
          lc_return         TYPE bapiret2_t,
          lv_perfil         TYPE string.

    r_user_perfil = me.

    READ TABLE me->zif_user_perfil~at_activitygroups INTO DATA(wa_activitygroups) WITH KEY agr_name = i_funcao.
    IF sy-subrc IS INITIAL.

      lv_perfil = CONV #( i_funcao ).
      DATA(v_message_erro) = |Perfil { lv_perfil } já atribuído ao usuário { me->zif_user_perfil~at_user }.|.
      WRITE v_message_erro.

***      RAISE EXCEPTION TYPE ZCX_USER_PERFIL
***        EXPORTING
***          TEXTID = VALUE #( MSGID = ZCX_USER_PERFIL=>ZCX_FUNCAO_ATRIBUIDA-MSGID
***                            MSGNO = ZCX_USER_PERFIL=>ZCX_FUNCAO_ATRIBUIDA-MSGNO
***                            ATTR1 = CONV #( I_FUNCAO ) )
***          MSGID  = ZCX_USER_PERFIL=>ZCX_FUNCAO_ATRIBUIDA-MSGID
***          MSGNO  = ZCX_USER_PERFIL=>ZCX_FUNCAO_ATRIBUIDA-MSGNO
***          MSGTY  = 'E'
***          MSGV1  = CONV #( I_FUNCAO ).

    ELSE.

      LOOP AT me->zif_user_perfil~at_activitygroups INTO wa_activitygroups.
        APPEND wa_activitygroups TO lc_activitygroups.
      ENDLOOP.

      APPEND i_funcao TO lc_activitygroups.

      CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
        EXPORTING
          username       = me->zif_user_perfil~at_user
        TABLES
          activitygroups = lc_activitygroups
          return         = lc_return.

      READ TABLE lc_return WITH KEY type = 'E' INTO DATA(wa_retorno).
      IF sy-subrc IS INITIAL.

        MESSAGE ID wa_retorno-id TYPE wa_retorno-type NUMBER wa_retorno-number
              INTO DATA(mtext)
              WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.

        me->zif_user_perfil~gera_erro_geral( i_texto = mtext ).
      ELSE.

        "A função não existe
        READ TABLE lc_return WITH KEY type = 'S' id = 'S#' number = '216' INTO wa_retorno.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_retorno-id TYPE wa_retorno-type NUMBER wa_retorno-number
                INTO mtext
                WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.

          me->zif_user_perfil~gera_erro_geral( i_texto = mtext ).
        ELSE.
          APPEND i_funcao TO me->zif_user_perfil~at_activitygroups.

          LOOP AT lc_return INTO wa_retorno.
            MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number
                          WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4
                          DISPLAY LIKE wa_retorno-type.
          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_user_perfil~set_atribuir_funcoes_delete.

    DATA: lc_activitygroups	TYPE rssbr_t_badi_bapiagr,
          lc_return         TYPE bapiret2_t,
          lv_perfil         TYPE string.

    r_user_perfil = me.

    READ TABLE me->zif_user_perfil~at_activitygroups INTO DATA(wa_activitygroups) WITH KEY agr_name = i_funcao.
    IF sy-subrc IS NOT INITIAL.

      lv_perfil = CONV #( i_funcao ).
      DATA(v_message_erro) = |Perfil { lv_perfil } não atribuído para o usuário { me->zif_user_perfil~at_user }.|.
      WRITE v_message_erro.

***      RAISE EXCEPTION TYPE ZCX_USER_PERFIL
***        EXPORTING
***          TEXTID = VALUE #( MSGID = ZCX_USER_PERFIL=>ZCX_FUNCAO_NAO_ATRIBUIDA-MSGID
***                            MSGNO = ZCX_USER_PERFIL=>ZCX_FUNCAO_NAO_ATRIBUIDA-MSGNO
***                            ATTR1 = CONV #( I_FUNCAO ) )
***          MSGID  = ZCX_USER_PERFIL=>ZCX_FUNCAO_NAO_ATRIBUIDA-MSGID
***          MSGNO  = ZCX_USER_PERFIL=>ZCX_FUNCAO_NAO_ATRIBUIDA-MSGNO
***          MSGTY  = 'E'
***          MSGV1  = CONV #( I_FUNCAO ).

    ELSE.

      lc_activitygroups = me->zif_user_perfil~at_activitygroups.
      DELETE lc_activitygroups WHERE agr_name EQ i_funcao-agr_name.

      CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
        EXPORTING
          username       = me->zif_user_perfil~at_user
        TABLES
          activitygroups = lc_activitygroups
          return         = lc_return.

      READ TABLE lc_return WITH KEY type = 'E' INTO DATA(wa_retorno).
      IF sy-subrc IS INITIAL.

        MESSAGE ID wa_retorno-id TYPE wa_retorno-type NUMBER wa_retorno-number
              INTO DATA(mtext)
              WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4.

        me->zif_user_perfil~gera_erro_geral( i_texto = mtext ).
      ELSE.
        DELETE me->zif_user_perfil~at_activitygroups WHERE agr_name EQ i_funcao-agr_name.

        LOOP AT lc_return INTO wa_retorno.
          MESSAGE ID wa_retorno-id TYPE 'S' NUMBER wa_retorno-number
                        WITH wa_retorno-message_v1 wa_retorno-message_v2 wa_retorno-message_v3 wa_retorno-message_v4
                        DISPLAY LIKE wa_retorno-type.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD ZIF_USER_PERFIL~SET_LIMPAR.

    R_USER_PERFIL = ME.

    CLEAR: ME->ZIF_USER_PERFIL~AT_USER,
           ME->ZIF_USER_PERFIL~AT_LOGONDATA,
           ME->ZIF_USER_PERFIL~AT_DEFAULTS,
           ME->ZIF_USER_PERFIL~AT_ADDRESS,
           ME->ZIF_USER_PERFIL~AT_COMPANY,
           ME->ZIF_USER_PERFIL~AT_SNC,
           ME->ZIF_USER_PERFIL~AT_REF_USER,
           ME->ZIF_USER_PERFIL~AT_ALIAS,
           ME->ZIF_USER_PERFIL~AT_UCLASS,
           ME->ZIF_USER_PERFIL~AT_LASTMODIFIED,
           ME->ZIF_USER_PERFIL~AT_ISLOCKED,
           ME->ZIF_USER_PERFIL~AT_IDENTITY,
           ME->ZIF_USER_PERFIL~AT_ADMINDATA,
           ME->ZIF_USER_PERFIL~AT_DESCRIPTION,
           ME->ZIF_USER_PERFIL~AT_PARAMETER,
           ME->ZIF_USER_PERFIL~AT_PROFILES,
           ME->ZIF_USER_PERFIL~AT_ACTIVITYGROUPS,
           ME->ZIF_USER_PERFIL~AT_ADDTEL,
           ME->ZIF_USER_PERFIL~AT_ADDFAX,
           ME->ZIF_USER_PERFIL~AT_ADDTTX,
           ME->ZIF_USER_PERFIL~AT_ADDTLX,
           ME->ZIF_USER_PERFIL~AT_ADDSMTP,
           ME->ZIF_USER_PERFIL~AT_ADDRML,
           ME->ZIF_USER_PERFIL~AT_ADDX400,
           ME->ZIF_USER_PERFIL~AT_ADDRFC,
           ME->ZIF_USER_PERFIL~AT_ADDPRT,
           ME->ZIF_USER_PERFIL~AT_ADDSSF,
           ME->ZIF_USER_PERFIL~AT_ADDURI,
           ME->ZIF_USER_PERFIL~AT_ADDPAG,
           ME->ZIF_USER_PERFIL~AT_ADDCOMREM,
           ME->ZIF_USER_PERFIL~AT_PARAMETER1,
           ME->ZIF_USER_PERFIL~AT_GROUPS,
           ME->ZIF_USER_PERFIL~AT_UCLASSSYS,
           ME->ZIF_USER_PERFIL~AT_SYSTEMS.

  ENDMETHOD.


  METHOD ZIF_USER_PERFIL~SET_USUARIO.

    DATA: LC_RETORNO TYPE BAPIRET2_T.

    R_USER_PERFIL = ME.

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        USERNAME       = I_USUARIO
        CACHE_RESULTS  = ABAP_FALSE
      IMPORTING
        LOGONDATA      = ME->ZIF_USER_PERFIL~AT_LOGONDATA
        DEFAULTS       = ME->ZIF_USER_PERFIL~AT_DEFAULTS
        ADDRESS        = ME->ZIF_USER_PERFIL~AT_ADDRESS
        COMPANY        = ME->ZIF_USER_PERFIL~AT_COMPANY
        SNC            = ME->ZIF_USER_PERFIL~AT_SNC
        REF_USER       = ME->ZIF_USER_PERFIL~AT_REF_USER
        ALIAS          = ME->ZIF_USER_PERFIL~AT_ALIAS
        UCLASS         = ME->ZIF_USER_PERFIL~AT_UCLASS
        LASTMODIFIED   = ME->ZIF_USER_PERFIL~AT_LASTMODIFIED
        ISLOCKED       = ME->ZIF_USER_PERFIL~AT_ISLOCKED
        IDENTITY       = ME->ZIF_USER_PERFIL~AT_IDENTITY
        ADMINDATA      = ME->ZIF_USER_PERFIL~AT_ADMINDATA
        DESCRIPTION    = ME->ZIF_USER_PERFIL~AT_DESCRIPTION
      TABLES
        PARAMETER      = ME->ZIF_USER_PERFIL~AT_PARAMETER
        PROFILES       = ME->ZIF_USER_PERFIL~AT_PROFILES
        ACTIVITYGROUPS = ME->ZIF_USER_PERFIL~AT_ACTIVITYGROUPS
        ADDTEL         = ME->ZIF_USER_PERFIL~AT_ADDTEL
        ADDFAX         = ME->ZIF_USER_PERFIL~AT_ADDFAX
        ADDTTX         = ME->ZIF_USER_PERFIL~AT_ADDTTX
        ADDTLX         = ME->ZIF_USER_PERFIL~AT_ADDTLX
        ADDSMTP        = ME->ZIF_USER_PERFIL~AT_ADDSMTP
        ADDRML         = ME->ZIF_USER_PERFIL~AT_ADDRML
        ADDX400        = ME->ZIF_USER_PERFIL~AT_ADDX400
        ADDRFC         = ME->ZIF_USER_PERFIL~AT_ADDRFC
        ADDPRT         = ME->ZIF_USER_PERFIL~AT_ADDPRT
        ADDSSF         = ME->ZIF_USER_PERFIL~AT_ADDSSF
        ADDURI         = ME->ZIF_USER_PERFIL~AT_ADDURI
        ADDPAG         = ME->ZIF_USER_PERFIL~AT_ADDPAG
        ADDCOMREM      = ME->ZIF_USER_PERFIL~AT_ADDCOMREM
        PARAMETER1     = ME->ZIF_USER_PERFIL~AT_PARAMETER1
        GROUPS         = ME->ZIF_USER_PERFIL~AT_GROUPS
        UCLASSSYS      = ME->ZIF_USER_PERFIL~AT_UCLASSSYS
        "EXTIDHEAD      = ME->ZIF_USER_PERFIL~AT_EXTIDHEAD
        "EXTIDPART      = ME->ZIF_USER_PERFIL~AT_EXTIDPART
        SYSTEMS        = ME->ZIF_USER_PERFIL~AT_SYSTEMS
        RETURN         = LC_RETORNO.

    ME->ZIF_USER_PERFIL~AT_USER = I_USUARIO.

    READ TABLE LC_RETORNO WITH KEY TYPE = 'E' INTO DATA(WA_RETORNO).
    CHECK SY-SUBRC IS INITIAL.

    ME->ZIF_USER_PERFIL~SET_LIMPAR( ).

    MESSAGE ID WA_RETORNO-ID TYPE WA_RETORNO-TYPE NUMBER WA_RETORNO-NUMBER
          INTO DATA(MTEXT)
          WITH WA_RETORNO-MESSAGE_V1 WA_RETORNO-MESSAGE_V2 WA_RETORNO-MESSAGE_V3 WA_RETORNO-MESSAGE_V4.

    ME->ZIF_USER_PERFIL~GERA_ERRO_GERAL( I_TEXTO = MTEXT ).

  ENDMETHOD.
ENDCLASS.
