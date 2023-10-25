#include <stdio.h>

#ifdef USE_TIRPC
#include <tirpc/rpc/types.h>
#else
#include <rpc/types.h>
#endif

/* Should confirm type to avoid symlink hack false positivies */
int main()
{
#ifdef _RPC_TYPES_H
    printf("rpc\n");
#endif

#ifdef _TIRPC_TYPES_H
    printf("tirpc\n");
#endif
    return 0;
}
