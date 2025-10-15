FUNCTION z_exibe_estrategia.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_OPERACAO) TYPE  ZFIWED001
*"----------------------------------------------------------------------

  FREE: it_0007,
        it_user,
        it_estrat.

  SELECT *
    FROM zfiwrt0007
    INTO TABLE it_0007
   WHERE operacao = i_operacao.

  IF it_0007[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há Estratégia de Liberação'.
    EXIT.
  ENDIF.

  SELECT *
    FROM user_addr
    INTO TABLE it_user
     FOR ALL ENTRIES IN it_0007
   WHERE bname = it_0007-usnam.

  SORT it_user BY bname.

  LOOP AT it_0007 INTO wa_0007.
    CLEAR wa_user.
    READ TABLE it_user INTO wa_user WITH KEY bname = wa_0007-usnam
                                    BINARY SEARCH.

    MOVE-CORRESPONDING wa_0007      TO wa_estrat.
    CONCATENATE wa_user-name_first
                wa_user-name_last INTO wa_estrat-nome SEPARATED BY space.

    APPEND wa_estrat                TO it_estrat.
  ENDLOOP.

*-exibir estrategia
  CALL SCREEN 100 STARTING AT 40   3
                    ENDING AT 150 15.

ENDFUNCTION.
