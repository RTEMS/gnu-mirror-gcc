/* { dg-do compile } */
/* { dg-additional-options "-Wno-cheri-explicit-pointer-conversion-from-cap -Wno-cheri-implicit-pointer-conversion-from-cap" } */

struct GTeth_desc
{
  unsigned ed_cmdsts;
};
struct GTeth_softc
{
  struct GTeth_desc txq_desc[32];
  unsigned int txq_fi;
  unsigned int txq_nactive;
};

void GTeth_txq_free (struct GTeth_softc *sc);

void
GTeth_txq_done (struct GTeth_softc *__capability sc)
{
  while (sc->txq_nactive > 0)
    {
      volatile struct GTeth_desc *txd
	= (volatile struct GTeth_desc *) &sc->txq_desc[sc->txq_fi];
      if (txd->ed_cmdsts)
	{
	  if (sc->txq_nactive == 1)
	    return;
	}
      GTeth_txq_free (sc);
    }
}
