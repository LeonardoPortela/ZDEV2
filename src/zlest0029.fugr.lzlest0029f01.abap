*----------------------------------------------------------------------*
***INCLUDE LZLEST0029F01 .
*----------------------------------------------------------------------*
FORM z_gera_seq_chave.

  DATA: lt_table TYPE TABLE OF zlest0029,
        st_table TYPE zlest0029,
        ltabix   TYPE i,
        lnewseq  TYPE i,
        lok.

  CHECK: zlest0029-numseqn IS INITIAL.
  lt_table[] = total[].

  SORT lt_table BY codposto codtrp.

  READ TABLE lt_table INTO st_table
    WITH KEY codposto = zlest0029-codposto
             codtrp   = zlest0029-codtrp
  BINARY SEARCH.

  ltabix  = sy-tabix.
  zlest0029-numseqn = '001'.

  CHECK: sy-subrc IS INITIAL.

  DO.
    lnewseq = st_table-numseqn.
    ADD 1 TO ltabix.
    READ TABLE lt_table INTO st_table INDEX ltabix.
    IF sy-subrc <> 0 OR
       st_table-codposto <> zlest0029-codposto OR
       st_table-codtrp   <> zlest0029-codtrp.
      EXIT.
    ENDIF.
  ENDDO.

  zlest0029-numseqn = lnewseq + 1.

ENDFORM.                    "Z_GERA_SEQ_CHAVE

*&---------------------------------------------------------------------*
*&      Form  Z_GERA_RENOVA_CHAVESEQ
*&---------------------------------------------------------------------*
FORM z_gera_renova_chaveseq.

  DATA: lt_table1    TYPE TABLE OF zlest0029,
        lt_table2    TYPE TABLE OF zlest0029,
        st_table1    TYPE zlest0029,
        st_table2    TYPE zlest0029,
        lnewseq      LIKE zlest0029-numseqn,
        lqb_codposto LIKE zlest0029-codposto,
        lqb_codtrp   LIKE zlest0029-codtrp,
        lseq_atual   TYPE i,
        lseq_old     TYPE i,
        ltabix1      TYPE i,
        ltabix2      TYPE i,
        lsubrc       TYPE i.

  CHECK: function = 'DELE'.

  DESCRIBE TABLE extract LINES lseq_atual.
  DESCRIBE TABLE total   LINES lseq_old.

  REFRESH: yt_dele.
  CHECK:  lseq_old > lseq_atual.

  lt_table1[] = total[].
  lt_table2[] = extract[].

  SORT: lt_table1 BY codposto codtrp numseqn,
        lt_table2 BY codposto codtrp numseqn.

  LOOP AT lt_table1 INTO st_table1.
    READ TABLE lt_table2 INTO st_table2
          WITH KEY codposto = st_table1-codposto
                   codtrp   = st_table1-codtrp
                   numseqn  = st_table1-numseqn
    BINARY SEARCH.

    lsubrc  = sy-subrc.
    ltabix2 = sy-tabix.

    IF lsubrc IS INITIAL.
      IF NOT lnewseq IS INITIAL AND
         st_table1-codposto = lqb_codposto AND
         st_table1-codtrp   = lqb_codtrp.

        yt_dele-codposto = st_table1-codposto.
        yt_dele-codtrp   = st_table1-codtrp.
        yt_dele-numseqn  = st_table1-numseqn.
        yt_dele-newseqn  = lnewseq.
        APPEND yt_dele.

        st_table2-numseqn = lnewseq.
        MODIFY lt_table2 INDEX ltabix2 FROM st_table2
                         TRANSPORTING numseqn.
        ADD 1 TO lnewseq.
      ENDIF.
    ENDIF.

    IF ( NOT lsubrc IS INITIAL ) AND
       ( st_table1-codposto <> lqb_codposto OR
         st_table1-codtrp   <> lqb_codtrp ).
      lqb_codposto = st_table1-codposto.
      lqb_codtrp   = st_table1-codtrp.
      lnewseq = st_table1-numseqn.
    ENDIF.

  ENDLOOP.

  extract[] = lt_table2[].

ENDFORM.                    "Z_GERA_RENOVA_CHAVESEQ

*&---------------------------------------------------------------------*
*&      Form  Z_CHECK_KEYS
*&---------------------------------------------------------------------*
FORM z_check_keys.

  CHECK: NOT yt_dele[] IS INITIAL.

  LOOP AT yt_dele.
    UPDATE zlest0029
       SET numseqn = yt_dele-newseqn
     WHERE codposto = yt_dele-codposto
       AND codtrp   = yt_dele-codtrp
       AND numseqn  = yt_dele-numseqn.
  ENDLOOP.

ENDFORM.                    "Z_CHECK_KEYS
