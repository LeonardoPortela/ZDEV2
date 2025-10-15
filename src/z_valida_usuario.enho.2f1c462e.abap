"Name: \PR:RM07IDIF\FO:USER_COMMAND\SE:BEGIN\EI
ENHANCEMENT 0 Z_VALIDA_USUARIO.
*Valida se o usuario do SAP pode executar o botão "Registra Diferença" para os documentos que tem
*"Qtd. Diferença" diferente de zero.
  data: wl_setleaf type setleaf,
        wl_erro.

   clear: wl_erro, wl_setleaf.

  if sy-tcode eq 'MI20'.
    select single *
      from setleaf
      into wl_setleaf
       where setname eq 'MI20'
         and valfrom eq sy-uname.

    LOOP AT yiseg WHERE box   EQ 'X'
                      and DIFMG ne 0.

     IF WL_SETLEAF-VALFROM IS INITIAL.
       MOVE: SPACE TO YISEG-BOX.
       MODIFY YISEG.
       wl_erro = 'X'.
     ENDIF.
    endloop.
    if wl_erro is not initial.
      message i836(sd) display like 'E' with 'Há documentos, que não podem ser processados'
                                             'por este usuário.'.
    endif.
endif.
ENDENHANCEMENT.
