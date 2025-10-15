"Name: \PR:ZRM07MLBD\EX:BELEGE_ERGAENZEN_02\EI
ENHANCEMENT 0 ZENHMM115.
* BUSTW OIH1 only in rel 472
    IF  ( sy-subrc <> 0 AND NOT g_s_mseg_lean-bustw = 'OIH1').  "2099103
      DELETE                 g_t_mseg_lean.
    ELSE.                                                   "n443935
*     enrich the current entry with the company code        "n443935
      PERFORM f9300_read_organ                              "n443935
                   USING     c_werks  g_s_mseg_lean-werks.  "n443935
                                                            "n443935
      CHECK : sy-subrc IS INITIAL.                          "n443935
      MOVE  g_s_organ-bukrs  TO  g_s_mseg_lean-bukrs.       "n443935
      MODIFY  g_t_mseg_lean  FROM  g_s_mseg_lean            "n443935
                             TRANSPORTING  bukrs.           "n451923
    ENDIF.
ENDENHANCEMENT.
