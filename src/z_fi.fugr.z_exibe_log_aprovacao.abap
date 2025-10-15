FUNCTION z_exibe_log_aprovacao.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SEQ_LCTO) TYPE  ZFIWED006
*"----------------------------------------------------------------------

  FREE: it_0018,
        it_user,
        it_logaprov.

  SELECT *
    FROM zfiwrt0018
    INTO TABLE it_0018
   WHERE seq_lcto = i_seq_lcto.

  IF it_0018[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há Log de Aprovação'.
    EXIT.
  ENDIF.

  SELECT *
    FROM user_addr
    INTO TABLE it_user
     FOR ALL ENTRIES IN it_0018
   WHERE bname = it_0018-usnam.

  SORT it_user BY bname.

  LOOP AT it_0018 INTO wa_0018.
    CLEAR wa_user.
    READ TABLE it_user INTO wa_user WITH KEY bname = wa_0018-usnam
                                    BINARY SEARCH.

    MOVE-CORRESPONDING wa_0018      TO wa_logaprov.
    CONCATENATE wa_user-name_first
                wa_user-name_last INTO wa_logaprov-nome SEPARATED BY space.

    APPEND wa_logaprov              TO it_logaprov.
  ENDLOOP.

*-exibir log aprovacao
  CALL SCREEN 200 STARTING AT 40   3
                    ENDING AT 150 15.

ENDFUNCTION.
