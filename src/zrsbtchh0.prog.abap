report zrsbtchh0.

* Dispaly date in form DD/MM/YYYY. Changes are marked with "YYYY

tables: bhdgd, tlsep, tsp03, t022d, t000, tsp1d, tsp06, toadarc,
        toasp.
tables: dd03l, dd01v, dd04v.

data: begin of params.
        include structure pri_params.
data: end   of params.

data: begin of save_params.
        include structure pri_params.
data: end   of save_params.

data: begin of info.
        include structure bhinf.
data: end of info.

data: begin of arc_params.
        include structure arc_params.
data: end   of arc_params.

data: begin of arc_mem.
        include structure arc_params.
data: end   of arc_mem.

data: begin of belg_info occurs 10.      "fuer Referenz auf
        include structure toav0.         "Originalbeleg(e)
data: end of belg_info.

data: begin of object_key,
        bukrs type bukrs,
        belnr type belnr_d,
        gjahr type gjahr,
      end of object_key.

data: object_key_interface like sapb-sapobjid.

data: bhdgi_hf(1)     type c,
      bhdgi_bkold(4)  type c,
      bhdgi_butxt     type butxt,
      bhdgi_ort01     type ort01,
      bhdgi_index(2)  type p,
      bhdgi_index0(2) type p,
      bhdgi_index1(2) type p,
      bhdgi_index2(2) type p,
      bhdgi_replen    like sy-index,
      bhdgi_unmlen    like sy-index,
      bhdgi_rldlen    like sy-index,
      bhdgi_rldnr(2)  type c,
      bhdgi_rldtxt(13) type c,
      bhdgi_p         type p,
      bhdgi_t1(70)    type c,
      bhdgi_t2(60)    type c,
      bhdgi_ln        like pri_params-prdsn,
      bhdgi_ix(1)     type c,
      bhdgi_pc(1)     type c,
      bhdgi_pr(1)     type c,
      bhdgi_desti     like tlsep-desti,
      bhdgi_tx(79)    type c,
      bhdgi_mif(116)  type c,
      bhdgi_mandt(3)  type c value '000',
      retcode(1)      type c,
      xlayout         like tsp1d-papart,
      default_layout  like tsp1d-papart value 'X_65_132',
      linsz           like sy-linsz,
      linct           like sy-linct,
      valid           type c,
      sel_opt         like pri_params-prbig,
      not_keeps       like pri_params-prrel,
      ctext           like params-prtxt.

data: begin of ficheline,
        mandt  like bhdgd-mandt,
        datum  like bhdgd-datum,
        zeit   like bhdgd-zeit,
        repif  like bhdgd-repif,
        seqno  like bhdgd-seqno,
        reqst  like bhdgd-reqst,
        grpin  like bhdgd-grpin,
      end of ficheline.

data: printoff(1) type c value '0',  "Steuerung NEW-PAGE PRINT OFF.
      first,
      found type i,
      index_line(132).

data: begin of x001.
        include structure t001_bf.
data: end of x001.
data: x001z like t001z_bf occurs 0 with header line.

data: len_butxt type i,
      len_text1 type i,
      len_text2 type i,
      len_repid type i,
      len_usrid type i,
      len_rldnr type i,
      len_repid_usrid type i.

constants:
      fagl_bhdgd_rldnr(16) type c value 'FAGL_BHDGD_RLDNR'.

data: l_repid like bhdgd-repid.
data: l_pagno like sy-pagno.

data: d_offset   type i,
      offset_tab type abap_offset_tab,
      m_line     like bhdgd-line1.

* Memory-Length
data: begin of m_leng,
          butxt type i,
          text1 type i,
          ort01 type i,
          text2 type i,
          rld_text013 type i,
        end of m_leng.

* Display-Length
data: d_leng like m_leng.

* available for display
data: max_d_text1 type i,
      max_d_text2 type i.

*---------------------------------------------------------------------*
*  BATCH-HEADING-ROUTINE ---------------------------------------------*
*---------------------------------------------------------------------*
start-of-selection.
  write: text-100, text-101, text-102.
  stop.


*---------------------------------------------------------------------*
*       FORM BATCH-HEADING                                            *
*       keine USING-Parameter                                         *
*---------------------------------------------------------------------*
*       Die Routine erzeugt einen Standard-Seitenkopf fuer            *
*       Reports, die im Batch laufen.                                 *
*---------------------------------------------------------------------*
form batch-heading using p_pagina.

  if bhdgd-lines = 0.
    move '132' to bhdgd-lines.
  endif.
  if bhdgd-lines > 254.
    bhdgd-lines = 254.
  endif.
* Minimum width for header-line
  if bhdgd-lines < 60.
    bhdgd-lines = 60.
  endif.

  if bhdgi_pc = 'X' and bhdgi_pr ne 'X'.
    bhdgi_pr = 'X'.
    if sy-pri40 ne 'Y'.
      set blank lines on.
      write: space(1).
      set blank lines off.
    else.
      write: space(1).
    endif.
  endif.

*-if(bhdgd-inifl= '0')--------------------------------------------------
  if bhdgd-inifl = '0'                            " Interne Meldung
  or bhdgd-inifl is initial.                      " Nr. 24944/2000

    bhdgd-inifl = '1'.
    bhdgi_mandt = sy-mandt.
    bhdgi_bkold = space.
    bhdgi_t1 = bhdgd-line1.
    bhdgi_t2 = bhdgd-line2.
    bhdgd-line1 = space.
    bhdgd-line2 = space.

*...import ledger from memory for standard page header
    import fagl_bhdgd_rldnr to bhdgi_rldnr
           from memory id fagl_bhdgd_rldnr.

    if not ( bhdgi_rldnr is initial ).
      concatenate text-013 bhdgi_rldnr
             into bhdgi_rldtxt separated by space.
      bhdgi_rldlen = strlen( bhdgi_rldtxt ) + 1.
      free memory id fagl_bhdgd_rldnr.
    endif.

  endif.
*-endif(bhdgd-inifl= '0') ----------------------------------------------

* Appendage of 1. and 2. Line
  if bhdgd-bukrs = space.
    bhdgd-bukrs = '0000'.
  endif.

  if bhdgi_bkold ne bhdgd-bukrs.
*---if(bhdgd-separ<>' ') -----------------------------------------------
    if bhdgd-separ ne ' '.
      bhdgi_bkold =  bhdgd-bukrs.
*-----if(retcode<>'0') -------------------------------------------------
      if retcode ne '0'.
        bhdgi_bkold =  bhdgd-bukrs.
        call function 'FI_COMPANYCODE_GETDETAIL'            "#EC EXISTS
             exporting  bukrs_int                  = bhdgi_bkold
                        authority_check            = space
             importing  t001_int                   = x001
             tables     t001z_int                  = x001z
             exceptions bukrs_not_found            = 1
                        no_authority_display_bukrs = 2
                        others                     = 3.
        if sy-subrc = 0.
          move x001-butxt to bhdgi_butxt.
          move x001-ort01 to bhdgi_ort01.
        else.
*         Kein Buchungskreis gefunden ?
          read table t000 with key bhdgi_mandt.
          move t000-mtext to bhdgi_butxt.
          move t000-ort01 to bhdgi_ort01.
        endif.
      endif.
*-----endif(retcode<>'0') ----------------------------------------------
      sy-pagno = 1.
    else. "if(BHDGD-SEPAR<>' ')
      bhdgi_bkold =  bhdgd-bukrs.
      call function 'FI_COMPANYCODE_GETDETAIL'              "#EC EXISTS
           exporting  bukrs_int                  = bhdgi_bkold
                      authority_check            = space
           importing  t001_int                   = x001
           tables     t001z_int                  = x001z
           exceptions bukrs_not_found            = 1
                      no_authority_display_bukrs = 2
                      others                     = 3.
      if sy-subrc = 0.
        move x001-butxt to bhdgi_butxt.
        move x001-ort01 to bhdgi_ort01.
      else.
        read table t000 with key bhdgi_mandt.
        move t000-mtext to bhdgi_butxt.
        move t000-ort01 to bhdgi_ort01.
      endif.
*     SY-PAGNO = 1.
    endif.
*---endif(bhdgd-separ<>' ') -------------------------------------------
  endif.

  if l_pagno is initial.
    l_pagno = p_pagina.
  endif.

*  IF SY-PAGNO = 0.
*    SY-PAGNO = 1.
*  ENDIF.
*  l_pagno = p_pagina - 1.
*  IF BHDGD-START_PAGNO > 0.
*    L_PAGNO = BHDGD-START_PAGNO + p_pagina.
*  ELSE.
*    l_pagno = l_pagno + 1.
*  ENDIF.

* Ausgabe Micro-Fiche Information
  case bhdgd-miffl.
    when ' '.
      "Dummy
    when 'X'.
      bhdgi_p = sy-mandt.
      unpack bhdgi_p to bhdgd-mandt.
      bhdgd-datum = sy-datlo.
      bhdgd-zeit  = sy-timlo.
      if bhdgd-repif is initial.
        bhdgd-repif = bhdgd-repid.
      endif.
      bhdgd-seqno = '001'.
      bhdgd-reqst = bhdgd-uname.
      move-corresponding bhdgd to ficheline.
*     CONDENSE FICHELINE.
      write ficheline.
    when 'A'.
      bhdgi_p = sy-mandt.
      unpack bhdgi_p to bhdgd-mandt.
      bhdgd-datum = sy-datlo.
      bhdgd-zeit  = sy-timlo.
      if bhdgd-repif is initial.
        bhdgd-repif = bhdgd-repid.
      endif.
      bhdgd-seqno = '001'.
      bhdgd-reqst = bhdgd-uname.
      move-corresponding bhdgd to ficheline.
*     WRITE: '%%%MCDAIN',10 FICHELINE.
      clear index_line.
      write: 'DAIN' to index_line.
      write: ficheline to index_line+4.
      print-control index-line index_line.
  endcase.

* Changes for Unicode
*=====================
* length of report ID (maximum is 40) and uname ID (maximum is 12).
* together at least of length 17, including the separator '/'
  l_repid   = bhdgd-repid.
  len_repid = strlen( bhdgd-repid ).
  len_usrid = strlen( bhdgd-uname ).
  if len_usrid = 0 and len_repid < 17.
    len_repid = 17.
  endif.
  len_repid_usrid = len_repid + len_usrid.
  if len_repid_usrid < 16.
    if len_repid >= 8.
      len_usrid = 16 - len_repid.
    else.
      len_repid = len_repid + ( 16 - len_repid_usrid ) / 2.
      len_usrid = 16 - len_repid.
    endif.
  endif.

  m_leng-butxt = strlen( bhdgi_butxt ).
  call method cl_abap_list_utilities=>display_offset
    exporting
      field          = bhdgi_butxt
      memory_offset  = m_leng-butxt
    importing
      display_offset = d_leng-butxt.
  if d_leng-butxt < m_leng-butxt.                           "1084000
    d_leng-butxt = m_leng-butxt.                            "1084000
  endif.                                                    "1084000

  m_leng-ort01 = strlen( bhdgi_ort01 ).
  call method cl_abap_list_utilities=>display_offset
    exporting
      field          = bhdgi_ort01
      memory_offset  = m_leng-ort01
    importing
      display_offset = d_leng-ort01.
  if d_leng-ort01 < m_leng-ort01.                           "1084000
    d_leng-ort01 = m_leng-ort01.                            "1084000
  endif.                                                    "1084000

  m_leng-text1 = strlen( bhdgi_t1 ).
  call method cl_abap_list_utilities=>display_offset
    exporting
      field          = bhdgi_t1
      memory_offset  = m_leng-text1
    importing
      display_offset = d_leng-text1.
  if d_leng-text1 < m_leng-text1.                           "1084000
    d_leng-text1 = m_leng-text1.                            "1084000
  endif.                                                    "1084000

  m_leng-text2 = strlen( bhdgi_t2 ).
  call method cl_abap_list_utilities=>display_offset
    exporting
      field          = bhdgi_t2
      memory_offset  = m_leng-text2
    importing
      display_offset = d_leng-text2.
  if d_leng-text2 < m_leng-text2.                           "1084000
    d_leng-text2 = m_leng-text2.                            "1084000
  endif.                                                    "1084000

  m_leng-rld_text013 = strlen( text-013 ).
  call method cl_abap_list_utilities=>display_offset
    exporting
      field          = text-013
      memory_offset  = m_leng-rld_text013
    importing
      display_offset = d_leng-rld_text013.
  if d_leng-rld_text013 < m_leng-rld_text013.               "1084000
    d_leng-rld_text013 = m_leng-rld_text013.                "1084000
  endif.                                                    "1084000

* Same size for BUTXT and ORT01
  if d_leng-ort01 > d_leng-butxt.
    d_leng-butxt = d_leng-ort01.
  endif.
  d_leng-ort01 = d_leng-butxt.

  if bhdgd-lines > 85.
*   normal list size
*   1. Line
    max_d_text1 = bhdgd-lines - ( d_leng-butxt + 1 )
                   - ( 1 + 17 ) - ( 1 + 16 ).

*   2. Line
    max_d_text2 = bhdgd-lines - ( d_leng-ort01 + 1 ) - ( 1 + 16 ).
    max_d_text2 = max_d_text2 - ( len_repid + 1 ).
    if len_usrid > 0.
      max_d_text2 = max_d_text2 - ( len_usrid + 2 ).
    endif.
  else.
*   small list size
*   1. Line without time
    max_d_text1 = bhdgd-lines - ( d_leng-butxt + 1 ) - ( 1 + 16 ).

*   2. Line without repid/usrid
    max_d_text2 = max_d_text1.
  endif.

* Fill line 1
  clear m_line.
  refresh offset_tab.

  write bhdgi_butxt to m_line+10(d_leng-butxt).

* Center the text
  if d_leng-text1 > max_d_text1.
    d_leng-text1 = max_d_text1.
  endif.
  d_offset = d_leng-butxt + 10 + 1 + ( max_d_text1 - d_leng-text1 ) / 2.
  append d_offset to offset_tab.
  write bhdgi_t1 to m_line+d_offset(d_leng-text1).

  if bhdgd-lines > 85.
*    d_offset = bhdgd-lines - ( 17 + 1 ) - 16.
*    APPEND d_offset TO offset_tab.
*    WRITE text-001 TO m_line+d_offset(4).

*    d_offset = d_offset + 4 + 1.
*    append d_offset to offset_tab.
*    WRITE sy-timlo USING EDIT MASK '__:__:__' TO m_line+d_offset(08).
  endif.

*  d_offset = bhdgd-lines - 16.
*  APPEND d_offset TO offset_tab.
*  WRITE text-002 TO m_line+d_offset(05).

*  d_offset = d_offset + 5 + 1.
*  APPEND d_offset TO offset_tab.
*  WRITE sy-datlo DD/MM/YYYY TO m_line+d_offset(10).

  call method cl_abap_list_utilities=>memory_to_display
    exporting
      memory_data  = m_line
      offset_tab   = offset_tab
    importing
      display_data = bhdgd-line1.

* Fill line 2
  clear m_line.
  refresh offset_tab.

  write bhdgi_ort01 to m_line+10(d_leng-ort01).

* Info for ledger in line 2
  if not ( bhdgi_rldnr is initial ).
    max_d_text2 = max_d_text2 - 1 - ( d_leng-rld_text013 + 3 ).

*    d_offset = d_leng-ort01 + 12.
*    append d_offset to offset_tab.
*    write text-013 to m_line+d_offset(d_leng-rld_text013).
*
*    d_offset = d_offset + d_leng-rld_text013 + 1.
*    append d_offset to offset_tab.
*    write bhdgi_rldnr to m_line+d_offset(2).
*
*    d_offset = d_offset + 2.
  else.
    d_offset = d_leng-ort01.
  endif.

* Shorten repid/usrid if this is necessary and abbreviate by '...'
*  if max_d_text2 < 0.
*    len_repid = len_repid + max_d_text2 - 3.
*    l_repid+len_repid(3) = '...'.
*    len_repid = len_repid + 3.
*    max_d_text2 = 0.
*  endif.

* Center the text
*  if d_leng-text2 > max_d_text2.
*    d_leng-text2 = max_d_text2.
*  endif.
*  d_offset = d_offset + 1 + ( max_d_text2 - d_leng-text2 ) / 2.
*  append d_offset to offset_tab.
  write bhdgi_t2 to m_line+d_offset(d_leng-text2).

  if bhdgd-lines > 85.
    d_offset = bhdgd-lines.
    d_offset = d_offset - ( len_repid + 1 ).
    if len_usrid > 0.
      d_offset = d_offset - ( len_usrid + 1 ).
    endif.
    append d_offset to offset_tab.
*    WRITE l_repid TO m_line+d_offset(len_repid).
    if len_usrid > 0.
*      d_offset = d_offset + len_repid.
*      WRITE: '/'  TO  m_line+d_offset.
*      d_offset = d_offset + 1.
*      WRITE: bhdgd-uname TO m_line+d_offset.
    endif.
  endif.

*  d_offset = bhdgd-lines - 16.
*  APPEND d_offset TO offset_tab.
  write: text-003 to m_line+d_offset(6).

  d_offset = bhdgd-lines - 9.
  append d_offset to offset_tab.
  write: l_pagno to m_line+d_offset(8) no-sign.
  l_pagno = l_pagno + 1.
  call method cl_abap_list_utilities=>memory_to_display
    exporting
      memory_data  = m_line
      offset_tab   = offset_tab
    importing
      display_data = bhdgd-line2.

* OPEN FI PROCESS 00003210
  perform output_openfi.

* Ausgabe Kopfzeile 1 und 2.
  perform bhdg_print using bhdgd-line1 bhdgd-lines.
  perform bhdg_print using bhdgd-line2 bhdgd-lines.

endform. "batch-heading


*---------------------------------------------------------------------*
*       FORM BHDG_PRINT                                               *
*---------------------------------------------------------------------*
*       Ausgabe der Kopfzeilen 1 und 2. Die Länge der Kopzeile ist    *
*       Abhängig von der Reportbreite.                                *
*---------------------------------------------------------------------*
form bhdg_print using linex lines.

  field-symbols <f1>.

  assign linex to <f1>.
  if lines < 255.
    assign <f1>(lines) to <f1>.
  endif.
  write: /1 <f1>.

endform. "bhdg_print

*---------------------------------------------------------------------*
*       FORM JUST_ROUTINE                                             *
*---------------------------------------------------------------------*
* Die Routine justiert einen Text, der im Feld J-LINE uebergeben      *
* wird, in die Mitte des Feldes J-LINE. Im Parameter J-LENGTH         *
* ist die Laenge des Feldes zu uebergeben                             *
*---------------------------------------------------------------------*
form just_routine using j_line j_length.

  data: just_line(255) type c,
        just_index(2)  type p,
        just_hf(1)     type c.

  if bhdgd-lines = 0. move '132' to bhdgd-lines. endif.

  just_line = j_line.

* Entfernen der vor dem Text stehenden Blanks in PH-LINE
  just_index = 0.
  do 255 times
     varying just_hf from just_line+0(1) next just_line+1(1)
                                        range just_line.
    if just_hf <> space.
      exit.
    endif.
    just_index = sy-index.
  enddo.
  shift just_line  by just_index places.

* Zentrierung von PH-LINE in die Mitte der Ausgabezeile
  just_index = 0.
  just_hf    = space.

  do 255 times
     varying just_hf from just_line+254(1) next just_line+253(1)
                                          range just_line.
    if just_hf <> space.
      exit.
    endif.
    just_index = sy-index.
  enddo.

  just_index = j_length + just_index - 255.
  just_index = just_index / 2.
  shift just_line  by just_index places right.
  j_line = just_line.

endform. "just_routine

*---------------------------------------------------------------------*
*       FORM NEW-SECTION                                              *
*---------------------------------------------------------------------*
*       Besorgen der Listseparations-Parameter                        *
*---------------------------------------------------------------------*
form new-section.

* Setzen Anzahl Seiten /Spalten wenn nicht definiert
* auf Standardausgabeformat.
  if sy-linsz eq 0.
    linsz = 132.
  else.
    linsz = sy-linsz.
  endif.

  if sy-linct eq 0.
    linct = 58.
  else.
    linct = sy-linct.
  endif.

* Ausgabe auf Drucker nur bei PF13 (Ausf. und Drucken)
  if sy-batch <> 'X'.
    if sy-pdest = space.
      new-page.
      exit.
    endif.
  endif.

* Merken der interaktiv eingegebenen Druckparameter beim ersten Aufruf.
* Diese werden verwendet, wenn in TLSEP kein Eintrag gefunden wird.
  if first <> 'X'.
    save_params = %_print.
    first = 'X'.
  endif.

*-if(bhdgd-separ<>' ')--------------------------------------------------
  if bhdgd-separ ne ' '.
    if bhdgi_ix = space.
      bhdgi_ix = 'X'.
      bhdgi_ln = sy-dsnam.
    endif.
    tlsep-domai = bhdgd-domai.
    tlsep-werte = bhdgd-werte.
    read table tlsep.
*---if(sy-subrc=0)------------------------------------------------------
    if sy-subrc eq 0.
      bhdgi_butxt = tlsep-ftex1.
      bhdgi_ort01 = tlsep-ftex2.
      retcode     = '0'.

      if bhdgi_desti ne space and
*        TLSEP-SPOOL NE SPACE AND
         tlsep-desti ne bhdgi_desti.
        print-control function 'RESET'.
        clear bhdgi_desti.
      endif.

      if printoff <> '0'.
        new-page print off.
      endif.

      xlayout = tlsep-layot.

*     siehe Funktionsbaustein RSPO_TEST_LAYOUT
      select single * from tsp03 where padest = tlsep-desti.
      select single * from tsp06 where pdptype = tsp03-patype
                                 and   pdpaper = xlayout
                                 and   pdname  = 'SETUP'
                                 and   pdlfdnr = 1.

      if sy-subrc <> 0.
        xlayout = default_layout.
        select single * from tsp03 where padest = tlsep-desti.
        select single * from tsp06 where pdptype = tsp03-patype
                                   and   pdpaper = xlayout
                                   and   pdname  = 'SETUP'
                                   and   pdlfdnr = 1.
      endif.

      if sy-subrc ne 0.
        xlayout = space.
      endif.

      if tlsep-cpage <> space.
        sel_opt = 'X'.
      else.
        sel_opt = ' '.
      endif.

      if tlsep-keeps <> space.
        not_keeps = ' '.
      else.
        not_keeps = 'X'.
      endif.

      ctext = tlsep-ctext.

      call function 'GET_PRINT_PARAMETERS'
        exporting
          line_count     = linct
          line_size      = linsz
          layout         = xlayout
          data_set       = tlsep-listd
          sap_cover_page = tlsep-cpage
          cover_page     = sel_opt
          list_text      = ctext
          receiver       = tlsep-recei
          destination    = tlsep-desti
          list_name      = tlsep-listn
          authority      = tlsep-autho
          new_list_id    = tlsep-nllid
          immediately    = tlsep-sofor
          release        = not_keeps
          expiration     = tlsep-daexp
          no_dialog      = 'X'
        importing
          out_parameters = params
          valid          = valid.

      new-page print off.                                   "672027
      new-page print on no dialog parameters params.

      printoff = '1'.

      if tlsep-splfo ne space.  "AND TLSEP-SPOOL NE SPACE.
        if tlsep-desti ne space.
          select single * from tsp03 where padest eq tlsep-desti.
          if sy-subrc = 0.

            if tsp03-patype <> space.
              t022d-typ = tsp03-patype.
              t022d-prctl = tlsep-splfo.
              read table t022d.
              if sy-subrc = 0.
                print-control function tlsep-splfo.
                bhdgi_desti = tlsep-desti.
*               SKIP TO LINE 1.
                bhdgi_pc = 'X'.
                bhdgi_pr = space(1).
              endif.
            endif.

          endif.
        endif.

      endif. "if(tlsep-splfo)

    else. "if(sy-subrc)

      if bhdgi_desti ne space.
        print-control function 'RESET'.
      endif.
      write text-004 to bhdgi_tx.
      replace '&DOMAI' with bhdgd-domai into bhdgi_tx.
      replace '&WERTE' with bhdgd-werte into bhdgi_tx.

      if printoff <> '0'.
        new-page print off.
      endif.

      ctext = bhdgi_tx.

      call function 'GET_PRINT_PARAMETERS'
        exporting
          line_count     = linct
          line_size      = linsz
          data_set       = bhdgi_ln
          sap_cover_page = 'X'
          cover_page     = 'X'
          list_text      = ctext
          new_list_id    = 'X'
          no_dialog      = 'X'
          in_parameters  = save_params
        importing
          out_parameters = params
          valid          = valid.

      new-page print off.                                   "672027
      new-page print on no dialog parameters params.

      printoff = '1'.
      retcode = '4'.
      new-page.
    endif.
*---endif(sy-subrc) ----------------------------------------------------

  else. "if(bhdgd-separ<>' ')

    if bhdgd-miffl <> 'A'.
      if bhdgi_pc ne 'X'.
        tlsep-domai = bhdgd-domai.
        tlsep-werte = bhdgd-werte.
        read table tlsep.
        if sy-subrc eq 0 and tlsep-splfo ne space.
          print-control function tlsep-splfo.
          bhdgi_pc = 'X'.
        endif.
      endif.
*   SY-PAGNO = 0.
      new-page.
    else. "Ausgabe auf Archiv
*     Die Umleitung auf das Archiv erfolgte bereits bei der Ausgabe
*     des Prologs mit START_ARCHIVE
      new-page.
    endif.
  endif.
*-endif(bhdgd-separ<>' ') ----------------------------------------------

endform.                    "NEW-SECTION

*&---------------------------------------------------------------------*
*&      Form  BATCH-HEADING1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->STRUCT     text
*----------------------------------------------------------------------*
form batch-heading1 using struct.
  bhdgd = struct.
  perform batch-heading using sy-pagno.
  struct = bhdgd.
endform.                    "BATCH-HEADING1

*&---------------------------------------------------------------------*
*&      Form  NEW-SECTION1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->STRUCT     text
*----------------------------------------------------------------------*
form new-section1 using struct.
  bhdgd = struct.
  perform new-section.
  struct = bhdgd.
endform.                    "NEW-SECTION1

*&---------------------------------------------------------------------*
*&      Form  OPEN_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->STRUCT     text
*----------------------------------------------------------------------*
form open_archive using struct.
  if bhdgd-miffl <> 'A'.
    exit.
  endif.
  arc_mem = struct.
endform.                    "OPEN_ARCHIVE

*&---------------------------------------------------------------------*
*&      Form  CLOSE_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form close_archive.
  if bhdgd-miffl <> 'A'.
    exit.
  endif.
  new-page print off.
endform.                    "CLOSE_ARCHIVE

*&---------------------------------------------------------------------*
*&      Form  START_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN         text
*----------------------------------------------------------------------*
form start_archive tables in structure info.

  data: name(40), ln(2).
  data: oa_pos(3) value '40'.

  if bhdgd-miffl <> 'A'.
    exit.
  endif.

* Ausgabe auf Spool nur bei PF13 (Ausf. und Drucken)
  if sy-batch <> 'X' and sy-pdest = space.
    new-page.
    sy-pagno = 1.
*   EXIT.
  else.
    if sy-batch = 'X'.
*     Starten im Hintergrund (mit Druckparametern)
      call function 'GET_PRINT_PARAMETERS'
        exporting
          in_archive_parameters  = arc_mem
          new_list_id            = 'X'
          no_dialog              = 'X'
          layout                 = 'X_65_132'
          mode                   = 'CURRENT'
        importing
          out_parameters         = params
          out_archive_parameters = arc_params
          valid                  = valid.
    else.
*     Starten am Selektionsbildschirm (Auf. und Drucken)
      call function 'GET_PRINT_PARAMETERS'
        exporting
          archive_mode           = '2'
          in_archive_parameters  = arc_mem
          immediately            = 'X'
          new_list_id            = 'X'
          no_dialog              = 'X'
          layout                 = 'X_65_132'
          mode                   = 'CURRENT'
        importing
          out_parameters         = params
          out_archive_parameters = arc_params
          valid                  = valid.
    endif.
    if params-pdest is initial.
      move 'LP01' to params-pdest.
    endif.
    new-page print off.                                     "672027
    new-page print on no dialog parameters params
                                archive parameters arc_params.
  endif.
* WRITE: / '%%%MCDPRL'.
  print-control index-line 'DPRL'.
* WRITE: / 'Mandant'(010), 30 SY-MANDT.
  clear index_line.
  write: 'Mandant'(010) to index_line.
  write: sy-mandt to index_line+29.
  print-control index-line index_line.
* WRITE: / 'Benutzer'(011), 30 SY-UNAME.
  clear index_line.
  write: 'Benutzer'(011) to index_line.
  write: sy-uname to index_line+29.
  print-control index-line index_line.
* WRITE: / 'Datenbank'(012), 30 SY-DBNAM.
  clear index_line.
  write: 'Datenbank'(012) to index_line.
  write: sy-dbnam to index_line+29.
  print-control index-line index_line.

  name = text-010.
* WRITE: / '%%%MCDKEY', 10 NAME, 55 '00', ' 03'.
  clear index_line.
  write: 'DKEY' to index_line.
  write: name to index_line+4.
  write: '00' to index_line+44.
  write: '03' to index_line+48.
  print-control index-line index_line.

  loop at in.
    name = '????'. ln = '00'.
    if in-len <> space.
      name = in-name.
      unpack in-len to ln. "Erzeugung führender Nullen
    else.
*     Feld -> Datenelement
      select * from dd03l where tabname   = in-name(4)
                          and   fieldname = in-name+5
                          and   as4local  = 'A'.
*       Datenelement -> Domäne
        select * from dd04v where rollname   = dd03l-rollname
                            and   ddlanguage = sy-langu.

          name =  dd04v-scrtext_l.

*         Nur mittels der Domäne läßt sich die korrekte Länge ermitteln
          select * from dd01v where domname =  dd04v-domname
                              and   ddlanguage = sy-langu.

            ln =  dd01v-outputlen+4(2).

*           Sonderbehandlung fuer Datum
            if dd04v-domname = 'DATUM'.
              ln = '08'.
            endif.

          endselect.
        endselect.
      endselect.
    endif.
*   WRITE: / '%%%MCDKEY', 10 NAME, 55 OA_POS, LN.
    clear index_line.
    write: 'DKEY' to index_line.
    write: name to index_line+4.
    write: oa_pos to index_line+44.
    write: ln to index_line+48.
    print-control index-line index_line.
    oa_pos = oa_pos + ln.
  endloop.

endform.                    "START_ARCHIVE

*&---------------------------------------------------------------------*
*&      Form  NEW-GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form new-group.

  if bhdgd-miffl = 'A'.
    bhdgi_p = sy-mandt.
    unpack bhdgi_p to bhdgd-mandt.
    bhdgd-datum = sy-datlo.
    bhdgd-zeit  = sy-timlo.
    if bhdgd-repif is initial.
      bhdgd-repif = bhdgd-repid.
    endif.
    bhdgd-seqno = '001'.
    bhdgd-reqst = bhdgd-uname.
    move-corresponding bhdgd to ficheline.
*   WRITE: '%%%MCDAIN',10 FICHELINE.
    clear index_line.
    write: 'DAIN' to index_line.
    write: ficheline to index_line+4.
    print-control index-line index_line.
  endif.

endform.                    "NEW-GROUP

*&---------------------------------------------------------------------*
*&      Form  END_ARCHIVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form end_archive.

  if bhdgd-miffl <> 'A'.
    exit.
  endif.

  new-page.
* WRITE: / '%%%MCDEPL'.
  print-control index-line 'DEPL'.
endform.                    "END_ARCHIVE

*&---------------------------------------------------------------------*
*&      Form  START_DOC_REFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->BUKRS      text
*      -->BELNR      text
*      -->GJAHR      text
*----------------------------------------------------------------------*
form start_doc_reference using bukrs belnr gjahr.
* Referenz auf Originalbeleg(e) s.a. Rep. OACONTA2 / Tab. TOADARC

  if bhdgd-miffl <> 'A'.
    exit.
  endif.

  object_key-bukrs = bukrs.
  object_key-belnr = belnr.
  object_key-gjahr = gjahr.
  object_key_interface = object_key.
  clear belg_info.
  refresh belg_info.
  call function 'ARCHIV_CONNECTINFO_GET_META'
    exporting
      ar_object    = space
      object_id    = object_key_interface
      sap_object   = 'BKPF'
    importing
      number       = found
    tables
      connect_info = belg_info
    exceptions
      others       = 4.
  if found > 0 and sy-subrc = 0.
    print-control function 'HYP<<'.
  else.
    found = 0.
  endif.

endform.                    "START_DOC_REFERENCE

*&---------------------------------------------------------------------*
*&      Form  END_DOC_REFERENCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form end_doc_reference.

  if bhdgd-miffl <> 'A'.
    exit.
  endif.

  if found > 0.
    print-control function 'HYP>>'.
    clear toasp.
    loop at belg_info.
      if toasp-ar_object <> belg_info-ar_object.
        select single * from toasp where ar_object = belg_info-ar_object
                                     and language = sy-langu.
        if sy-subrc <> 0.
          clear toasp.
        endif.
      endif.
      clear toadarc.
      toadarc-function = 'DARC'.
      toadarc-hypnumber = 0.
      toadarc-archiv_id = belg_info-archiv_id.
      toadarc-arc_doc_id = belg_info-arc_doc_id.
      toadarc-ar_date = belg_info-ar_date.
      toadarc-dokinfo = toasp-objecttext.
      print-control index-line toadarc.
    endloop.
  endif.

endform.                    "END_DOC_REFERENCE

*---------------------------------------------------------------------*
*    FORM SET_LANGUAGE USING SET_LANGU                                *
*---------------------------------------------------------------------*
form set_language using set_langu.
  if sy-langu <> set_langu.
    set language set_langu.
  endif.
endform.                    "SET_LANGUAGE

*---------------------------------------------------------------------*
*       FORM OUTPUT_OPENFI                                            *
*---------------------------------------------------------------------*
form output_openfi.
  call function 'FI_COMPANYCODE_GETDETAIL'                  "#EC EXISTS
       exporting  bukrs_int                  = bhdgd-bukrs
                  authority_check            = space
       importing  t001_int                   = x001
       tables     t001z_int                  = x001z
       exceptions bukrs_not_found            = 1
                  no_authority_display_bukrs = 2
                  others                     = 3.

  if sy-subrc = 0.
    call function 'FUNCTION_EXISTS'
      exporting
        funcname           = 'OPEN_FI_PERFORM_00003210_P'
      exceptions
        function_not_exist = 1
        others             = 2.

    if sy-subrc = 0.
      call function 'OPEN_FI_PERFORM_00003210_P'            "#EC EXISTS
           exporting i_land1 = x001-land1
           changing  i_bhdgd = bhdgd.
    endif.
  endif.
endform.                    "OUTPUT_OPENFI
