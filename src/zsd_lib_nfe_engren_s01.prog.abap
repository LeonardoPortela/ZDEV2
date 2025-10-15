************************************************************************
*  SELECTION SCREEN                                                    *
************************************************************************
selection-screen begin of block b1 with frame title text-001.

select-options:
               s_docnum for j_1bnfe_active-docnum matchcode object j1ba.
parameters:
  p_bukrs type bukrs obligatory default '1100',
  p_erro  type c no-display.
selection-screen end of block b1.
