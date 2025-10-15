PROCESS BEFORE OUTPUT.

  MODULE status_0200.

MODULE CRIAR_OBJETOS_0200.

  LOOP AT t_veicular INTO wa_veicular WITH CONTROL tbc0200.
    MODULE : preenche_screen_0200.
  ENDLOOP.

  LOOP AT t_contrato INTO wa_contrato WITH CONTROL tbc0300.
    MODULE : preenche_screen_0300.
  ENDLOOP.

PROCESS AFTER INPUT.

  CHAIN.
    FIELD wa_tela-chave_xml_cte
                                MODULE: limpa_tabelas ON REQUEST,
                                        zm_atualizar  ON REQUEST.
  ENDCHAIN.

  CHAIN.
    FIELD wa_tela-mot_cpf MODULE zm_motorista ON REQUEST.
    FIELD wa_tela-frota module zm_frota on REQUEST.
  ENDCHAIN.
*
    LOOP AT t_veicular.
    ENDLOOP.
*
    LOOP AT t_contrato.
    ENDLOOP.

         "IP_MODE = 'E'.

*    CLEAR OBJ.
*    OBJ-OBJTYPE = OBJTYPE.
*
*    OBJ-OBJKEY = wa_tela-chave_xml_cte.
*
*    CREATE OBJECT MANAGER
*      EXPORTING
*        IS_OBJECT        = OBJ
*        IP_NO_COMMIT     = 'R'
*        IP_MODE          = 'E' "IP_MODE
*      EXCEPTIONS
*        OBJECT_INVALID   = 1
*        CALLBACK_INVALID = 2
*        OTHERS           = 3.
*
*    IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1
*SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*
*    SET TITLEBAR 'T0100_2' WITH wa_tela-chave_xml_cte.

    MODULE user_command_0200.
