*----------------------------------------------------------------------*
***INCLUDE LZSD01F01 .
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

    CONCATENATE 'ZSD_LIM_CRED' VBELN INTO gn_fname.

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

  IMPORT VBELN TO VBELN FROM MEMORY ID 'ZSDWFP1'.

  CONCATENATE 'ZSD_LIM_CRED' VBELN INTO gn_fname.


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

*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_TEXTO_AP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ZF_BUSCA_TEXTO_AP .

  REFRESH tg_line2.
  CLEAR: wg_line2.

  IMPORT VBELN TO VBELN FROM MEMORY ID 'ZSDWFAP1'.

  CONCATENATE 'ZSD_LIM_CRED_AP' VBELN INTO gn_fname.


  gn_fid = 'ST'.
  gn_language = 'PT'.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = gn_fid
      language                = gn_language
      name                    = gn_fname
      object                  = 'TEXT'
    TABLES
      lines                   = tg_line2
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.


  loop at tg_line2.
    move tg_line2-tdline to wa_texttable2-line.
    append wa_texttable2-line to tg_texttable2.
  endloop.

*   Carregando tabela com dados a serem exibido
*   Texto a ser exibido - LÍDER
  CALL METHOD obj_texto2->set_selected_text_as_r3table
    EXPORTING
      table = tg_texttable2.


endform.                    " ZF_BUSCA_TEXTO_AP

*&---------------------------------------------------------------------*
*&      Form  ZRESGATA_TEXTO_CONTAINER_AP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ZRESGATA_TEXTO_CONTAINER_AP .

  refresh tg_line2.
  clear wg_line2.

* Resgata valores do container da tela
  CALL METHOD obj_texto2->get_text_as_r3table
    IMPORTING
      table = tg_texttable2.

*  IF tg_texttable[] IS INITIAL.
*    MESSAGE e011(z01) WITH 'Obrigatório informar Motivo da Rejeição'.
*
*  ELSE.

    LOOP AT tg_texttable2 into wa_texttable2.
      wg_line2-tdline = wa_texttable2-line.
      append wg_line2 to tg_line2.
    ENDLOOP.

    CONCATENATE 'ZSD_LIM_CRED_AP' VBELN INTO gn_fname.

    wa_thead2-tdobject = 'TEXT'.
    wa_thead2-tdname   = gn_fname.
    wa_thead2-tdid     = 'ST'.
    wa_thead2-tdspras  = 'PT'.

    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
*       CLIENT                = SY-MANDT
        header                = wa_thead2
       INSERT                = ' '
       SAVEMODE_DIRECT       = 'X'
*       OWNER_SPECIFIED       = ' '
*       LOCAL_CAT             = ' '
*     IMPORTING
*       FUNCTION              =
*       NEWHEADER             =
      tables
        lines                 = tg_line2
     EXCEPTIONS
       ID                    = 1
       LANGUAGE              = 2
       NAME                  = 3
       OBJECT                = 4
       OTHERS                = 5.


*  ENDIF.

endform.                    " ZRESGATA_TEXTO_CONTAINER_AP
