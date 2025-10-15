*----------------------------------------------------------------------*
***INCLUDE LZFI01F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ZRESGATA_TEXTO_CONTAINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ZRESGATA_TEXTO_CONTAINER .

  refresh tg_line.
  clear wg_line.

* Resgata valores do container da tela
  CALL METHOD obj_texto->get_text_as_r3table
    IMPORTING
      table = tg_texttable.

  IF tg_texttable[] IS INITIAL.
    MESSAGE e011(z01) WITH 'Obrigatório informar Motivo da Rejeição'.

  ELSE.

    LOOP AT tg_texttable into wa_texttable.
      wg_line-tdline = wa_texttable-line.
      append wg_line to tg_line.
    ENDLOOP.

    CONCATENATE 'ZFI_ADM_LIM_CRED' KKBER KUNNR INTO gn_fname.

    wa_thead-tdobject = 'TEXT'.
    wa_thead-tdname   = gn_fname.
    wa_thead-tdid     = 'ST'.
    wa_thead-tdspras  = 'PT'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT                = SY-MANDT
        header                = wa_thead
       INSERT                = ' '
       SAVEMODE_DIRECT       = 'X'
*       OWNER_SPECIFIED       = ' '
*       LOCAL_CAT             = ' '
*     IMPORTING
*       FUNCTION              =
*       NEWHEADER             =
      tables
        lines                 = tg_line
     EXCEPTIONS
       ID                    = 1
       LANGUAGE              = 2
       NAME                  = 3
       OBJECT                = 4
       OTHERS                = 5.

    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.



  ENDIF.

endform.                    " ZRESGATA_TEXTO_CONTAINER


*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_TEXTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ZF_BUSCA_TEXTO .


  REFRESH tg_line.
  CLEAR: wg_line.

  IMPORT KKBER TO KKBER FROM MEMORY ID 'ZFIWFP1'.
  IMPORT KUNNR TO KUNNR FROM MEMORY ID 'ZFIWFP2'.

  CONCATENATE 'ZFI_ADM_LIM_CRED' KKBER KUNNR INTO gn_fname.


  gn_fid = 'ST'.
  gn_language = 'PT'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = gn_fid
      language                = gn_language
      name                    = gn_fname
      object                  = 'TEXT'
    TABLES
      lines                   = tg_line
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.

  ENDIF.

  loop at tg_line.

    move tg_line-tdline to wa_texttable-line.
    append wa_texttable-line to tg_texttable.
  endloop.

*   Carregando tabela com dados a serem exibido
*   Texto a ser exibido - LÍDER
  CALL METHOD obj_texto->set_selected_text_as_r3table
    EXPORTING
      table = tg_texttable.

endform.                    " ZF_BUSCA_TEXTO
