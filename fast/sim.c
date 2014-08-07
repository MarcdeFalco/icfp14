#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define CELL_WALL 0
#define CELL_EMPTY 1
#define CELL_PILL 2
#define CELL_POWERPILL 3
#define CELL_FRUIT 4
#define CELL_LAMBDA_START 5
#define CELL_GHOST_START 6

int tickcount;
#define TICK_LAMBDA_NORMAL 127
#define TICK_LAMBDA_EATING 137
unsigned int tick_ghosts[] = { 130, 132, 134, 136 };
unsigned int tick_ghosts_fright[] = { 195, 198, 201, 204 };

char map[256][256];
unsigned int map_width;
unsigned int map_height;

void load_map(char *fn)
{
    FILE *f = fopen(fn, "r");

    int y = 0, x = 0;

    while (1)
    {
        char l[500];
        char c;

        if (feof(f)) break;

        x = 0;

        while ((c = fgetc(f)) != EOF) {
            if (c == '\n') break;

            int cell = 0;

            switch(c) {
            case '#': cell = CELL_WALL; break;
            case ' ': cell = CELL_EMPTY; break;
            case '.': cell = CELL_PILL; break;
            case 'o': cell = CELL_POWERPILL; break;
            case '%': cell = CELL_FRUIT; break;
            case '\\': cell = CELL_LAMBDA_START; break;
            case '=': cell = CELL_GHOST_START; break;
            default: break;
            }

            map[x][y] = cell;
            x++;

            if (x > map_width) map_width = x;
        }

        if (feof(f)) break;

        y++;
        if (y > map_height) map_height = y;
    }


    fclose(f);
}

#define GHC_ARG_REG 0
#define GHC_ARG_IREG 1
#define GHC_ARG_CONST 2
#define GHC_ARG_MEM 3

typedef struct {
    unsigned char type;
    unsigned char value;
} ghc_arg;

#define GHC_MOV 0
#define GHC_INC 1
#define GHC_DEC 2
#define GHC_ADD 3
#define GHC_SUB 4
#define GHC_MUL 5
#define GHC_DIV 6
#define GHC_AND 7
#define GHC_OR 8
#define GHC_XOR 9
#define GHC_JLT 10
#define GHC_JEQ 11
#define GHC_JGT 12
#define GHC_INT 13
#define GHC_HLT 14
#define GHC_OTHER 128
#define GHC_ERROR 129

typedef struct {
    unsigned char instr;
    ghc_arg arg1;
    ghc_arg arg2;
    unsigned char extra;
} ghc_instr;

void skip_space(FILE *f)
{
    char c = fgetc(f);
    while (c == ' ' || c == '\t') {
        c = fgetc(f);
    }
    ungetc(c,f);
}

ghc_arg read_ghc_arg(FILE *f)
{
    ghc_arg arg;

    char c = fgetc(f);
    char bindirect = 0;

    if  (c == '[') {
        bindirect = 1;
        skip_space(f);
        c = fgetc(f);
    }
    c = toupper(c);
    if (c >= 'A' && c <= 'H') {
        if (bindirect) arg.type = GHC_ARG_IREG;
        else arg.type = GHC_ARG_REG;
        arg.value = c - 'A';
        if (bindirect) { skip_space(f); fgetc(f); }
        return arg;
    }
    if (c == 'P') {
        fgetc(f);
        if (bindirect) arg.type = GHC_ARG_IREG;
        else arg.type = GHC_ARG_REG;
        arg.value = 8;
        if (bindirect) { skip_space(f); fgetc(f); }
        return arg;
    }
    ungetc(c, f);
    int v;
    if (bindirect) arg.type = GHC_ARG_MEM;
    else arg.type = GHC_ARG_CONST;
    fscanf(f, "%d", &v);
    arg.value = v;
    if (bindirect) { skip_space(f); fgetc(f); }
    return arg;
}

int read_int_arg(FILE *f)
{
        int v;
        fscanf(f, "%d", &v);
        return v;
}

ghc_instr read_ghc_instr(FILE *f)
{
    ghc_instr instr;
    char mnemo[10];
    char c = 0;
    unsigned char p = 0;
    unsigned char instr_type = GHC_ERROR;

    skip_space(f);

    while ( (c = fgetc(f)) != ' ' && c != '\t' && c != '\n' && c != EOF )
    {
        mnemo[p++] = toupper(c);
    }
    mnemo[p] = '\0';
    ungetc(c, f);

    if (strncmp(mnemo,"MOV",p) == 0) { instr_type = GHC_MOV; }
    if (strncmp(mnemo,"INC",p) == 0) { instr_type = GHC_INC; }
    if (strncmp(mnemo,"DEC",p) == 0) { instr_type = GHC_DEC; }
    if (strncmp(mnemo,"ADD",p) == 0) { instr_type = GHC_ADD; }
    if (strncmp(mnemo,"SUB",p) == 0) { instr_type = GHC_SUB; }
    if (strncmp(mnemo,"MUL",p) == 0) { instr_type = GHC_MUL; }
    if (strncmp(mnemo,"DIV",p) == 0) { instr_type = GHC_DIV; }
    if (strncmp(mnemo,"AND",p) == 0) { instr_type = GHC_AND; }
    if (strncmp(mnemo,"OR",p) == 0) { instr_type = GHC_OR; }
    if (strncmp(mnemo,"XOR",p) == 0) { instr_type = GHC_XOR; }
    if (strncmp(mnemo,"JLT",p) == 0) { instr_type = GHC_JLT; }
    if (strncmp(mnemo,"JEQ",p) == 0) { instr_type = GHC_JEQ; }
    if (strncmp(mnemo,"JGT",p) == 0) { instr_type = GHC_JGT; }
    if (strncmp(mnemo,"INT",p) == 0) { instr_type = GHC_INT; }
    if (strncmp(mnemo,"HLT",p) == 0) { instr_type = GHC_HLT; }

    if (instr_type == GHC_MOV ||
            (instr_type >= GHC_ADD && instr_type <= GHC_XOR))
    {
        skip_space(f);
        instr.arg1 = read_ghc_arg(f);
        skip_space(f);
        char c = fgetc(f);
        assert(c == ',');
        skip_space(f);
        instr.arg2 = read_ghc_arg(f);
    }
 
    if (instr_type == GHC_INC || instr_type == GHC_DEC)
    {
        skip_space(f);
        instr.arg1 = read_ghc_arg(f);
        skip_space(f);
    }

    if (instr_type >= GHC_JLT && instr_type <= GHC_JGT)
    {
        skip_space(f);
        instr.extra = (unsigned char) read_int_arg(f);
        skip_space(f);
        char c = fgetc(f);
        assert(c == ',');
        skip_space(f);
        instr.arg1 = read_ghc_arg(f);
        skip_space(f);
        c = fgetc(f);
        assert(c == ',');
        skip_space(f);
        instr.arg2 = read_ghc_arg(f);
    }
       
    if (instr_type == GHC_INT)
    {
        skip_space(f);
        instr.extra = read_int_arg(f);
        skip_space(f);
    }

    if (instr_type == GHC_HLT) skip_space(f);

    if (instr_type == GHC_ERROR) instr_type = GHC_OTHER;

    instr.instr = instr_type;

    return instr;
}

ghc_instr *load_ghc(char *fn)
{
    FILE *f = fopen(fn, "r");

    ghc_instr *code = (ghc_instr *)malloc(sizeof(ghc_instr) * 256);
    int i = 0;
    char c;

    while ( (c = fgetc(f)) != EOF )
    {
        ungetc(c, f);
        skip_space(f);

        c = fgetc(f);
        if (c == ';') { while( (c = fgetc(f)) != EOF && c != '\n' ); }
        if (c == EOF) break;
        if (c == '\n') continue;

        ungetc(c,f);

        code[i] = read_ghc_instr(f);

        if (code[i].instr == GHC_ERROR) {
            free(code);
            code = NULL;
            break;
        }

        i++;

        while( (c = fgetc(f)) != EOF && c != '\n' );
    }

    fclose(f);

    return code;
}

typedef struct {
    unsigned char regs[8];
    unsigned char pc;
    unsigned char data[256];
    ghc_instr *code;
} ghc_machine;

#define GHOST_VIT_STANDARD 0
#define GHOST_VIT_FRIGHT 1
#define GHOST_VIT_INVISIBLE 2

#define DIRECTION_UP 0
#define DIRECTION_RIGHT 1
#define DIRECTION_DOWN 2
#define DIRECTION_LEFT 3

#define ADVANCEX(x,d) (x - (d % 2) * (2 * (d / 2) - 1))
#define ADVANCEY(y,d) (y + ((d+1) % 2) * (2 * (d / 2) - 1))

/* Ugly hack by making everyone of the same size and 32bit aligned */
#define GCC_DATA_INT 0
#define GCC_DATA_CONS 1
#define GCC_DATA_CLOSURE 2

typedef struct {
    unsigned int type;
    int value;
    unsigned int unused;
} gcc_data;

typedef struct {
    unsigned int type;
    gcc_data *car;
    gcc_data *cdr;
} gcc_cons;

typedef struct gcc_frame_ {
    struct gcc_frame_ *parent;
    unsigned int size;
    gcc_data *locals;
    unsigned char dummy;
}  gcc_frame;

typedef struct {
    unsigned int type;
    unsigned int address;
    gcc_frame *frame;
} gcc_closure;

typedef struct {
    unsigned char instr;
    int arg1;
    int arg2;
} gcc_instr;

gcc_cons to_cons(gcc_data d)
{
    return *((gcc_cons*)&d);
}

void print_gcc_data(gcc_data d)
{
    if (d.type == GCC_DATA_INT)
        printf("%d", d.value);
    else if (d.type == GCC_DATA_CONS) {
        gcc_cons c = to_cons(d);
        putchar('(');
        print_gcc_data(*c.car);
        putchar(',');
        print_gcc_data(*c.cdr);
        putchar(')');
    } else {
        gcc_closure c = *((gcc_closure*)&d);
        printf("<%d,env>", c.address);
    }
}

#define STACK_SIZE 100000

#define GCC_CONTROL_RETURN 0
#define GCC_CONTROL_STOP 1
#define GCC_CONTROL_JOIN 2

typedef struct {
    unsigned int type;
    unsigned int address;
    gcc_frame *frame;
} gcc_control;

void print_gcc_control(gcc_control c)
{
    if (c.type == GCC_CONTROL_RETURN)
        printf("R%d", c.address);
    else if (c.type == GCC_CONTROL_JOIN)
        printf("J%d", c.address);
    else
        printf("Stop");
}

typedef struct {
    unsigned int pc;
    gcc_data *data_stack;
    gcc_data *data_stack_top;
    gcc_control *control_stack;
    gcc_control *control_stack_top;
    gcc_frame *frame;
    gcc_instr *code;
} gcc_machine;

typedef struct {
    unsigned int vit;
    unsigned char x;
    unsigned char y;
    unsigned char sx;
    unsigned char sy;
    unsigned char dir;
    unsigned int tick;
    unsigned int lives;
    unsigned int score;
    gcc_machine mac;
} lambdaman_t;

lambdaman_t lambdaman;

void print_gcc_machine()
{
    gcc_machine *mac = &lambdaman.mac;
    printf("PC : %d\n", mac->pc);
    printf("Data stack : ");
    gcc_data *d = mac->data_stack;
    while (d != mac->data_stack_top) {
        d--;
        print_gcc_data(*d);
        putchar('|');
    }
    putchar('\n');
    printf("Control stack : ");
    gcc_control *c = mac->control_stack;
    while (c != mac->control_stack_top) {
        c--;
        print_gcc_control(*c);
        if (c != mac->control_stack_top) putchar(' ');
    }
    putchar('\n');
    printf("Frames : [ ");
    for (int i = 0; i < mac->frame->size; i++)
    {
        print_gcc_data(mac->frame->locals[i]);
        putchar(' ');
    }
    printf("]\n\n");
}



typedef struct {
    unsigned char vit;
    unsigned char x;
    unsigned char y;
    unsigned char sx;
    unsigned char sy;
    unsigned char dir;
    unsigned char target_dir;
    unsigned int tick;
    ghc_machine mac;
} ghost;

ghost *ghosts;
unsigned char nghosts;

unsigned int fruit;

#define GHOST_RESULT_RUNNING 0
#define GHOST_RESULT_ERROR 1
#define GHOST_RESULT_STOP 2
unsigned char ghost_eval_res;

void ghc_init_machine(ghc_machine *mac, ghc_instr *code)
{
    memset(mac->regs, 0, 8);
    mac->pc = 0;
    memset(mac->data, 0, 256);
    mac->code = code;
}

unsigned char ghc_get(ghc_machine *mac, ghc_arg a)
{
    switch (a.type)
    {
    case GHC_ARG_REG:
        if (a.value < 8) return mac->regs[a.value];
        else return mac->pc;
    case GHC_ARG_IREG:
        if (a.value < 8) return mac->data[mac->regs[a.value]];
        else return mac->data[mac->pc];
    case GHC_ARG_CONST: return a.value;
    case GHC_ARG_MEM: return mac->data[a.value];
    default: ghost_eval_res = GHOST_RESULT_ERROR; return 0;
    }
}

void ghc_set(ghc_machine *mac, ghc_arg a, unsigned char v)
{
    switch (a.type)
    {
    case GHC_ARG_REG:
        if (a.value < 8) mac->regs[a.value] = v;
        else mac->pc = v;
        break;
    case GHC_ARG_IREG:
        if (a.value < 8) mac->data[mac->regs[a.value]] = v;
        else mac->data[mac->pc] = v;
        break;
    case GHC_ARG_CONST: ghost_eval_res = 1;
        break;
    case GHC_ARG_MEM: mac->data[a.value] = v;
        break;
    default: ghost_eval_res = 1;
    }
}
void ghost_eval_one_instr(unsigned char ghost_index)
{
    ghost *g = ghosts + ghost_index;
    ghc_machine *mac = &g->mac;
    ghc_instr i = mac->code[mac->pc];
    unsigned char old_pc = mac->pc;

    switch(i.instr) 
    {
    case GHC_MOV: ghc_set(mac, i.arg1, ghc_get(mac, i.arg2)); break;
    case GHC_INC: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)+1); break;
    case GHC_DEC: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)-1); break;
    case GHC_ADD: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)+ghc_get(mac, i.arg2)); break;
    case GHC_SUB: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)-ghc_get(mac, i.arg2)); break;
    case GHC_MUL: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)*ghc_get(mac, i.arg2)); break;
    case GHC_DIV: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)/ghc_get(mac, i.arg2)); break;
    case GHC_AND: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)&ghc_get(mac, i.arg2)); break;
    case GHC_OR: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)|ghc_get(mac, i.arg2)); break;
    case GHC_XOR: ghc_set(mac, i.arg1, ghc_get(mac, i.arg1)^ghc_get(mac, i.arg2)); break;
    case GHC_JLT: if (ghc_get(mac, i.arg1) < ghc_get(mac, i.arg2)) mac->pc = i.extra; break;
    case GHC_JEQ: if (ghc_get(mac, i.arg1) == ghc_get(mac, i.arg2)) mac->pc = i.extra; break;
    case GHC_JGT: if (ghc_get(mac, i.arg1) > ghc_get(mac, i.arg2)) mac->pc = i.extra; break;
    case GHC_INT : {
        switch (i.extra) {
        case 0 : 
            if (mac->regs[0] >= 0 && mac->regs[0] < 4)
                g->target_dir = mac->regs[0];
            break;
        case 1 : mac->regs[0]=lambdaman.x; mac->regs[1]=lambdaman.y; break;
        case 2 : ghost_eval_res = GHOST_RESULT_ERROR; break;
        case 3 : mac->regs[0] = ghost_index; break;
        case 4 : {
            ghost *go = &ghosts[mac->regs[0]];
            mac->regs[0]=go->sx; mac->regs[1]=go->sy;
            break;
        }
        case 5 : {
            ghost *go = &ghosts[mac->regs[0]];
            mac->regs[0]=go->x; mac->regs[1]=go->y;
            break;
        }
        case 6: {
            ghost *go = &ghosts[mac->regs[0]];
            mac->regs[0]=go->vit; mac->regs[1]=go->dir;
            break;
        }
        case 7: {
            mac->regs[0] = map[mac->regs[0]][mac->regs[1]];
            break;
        }
        case 8: break;
        default: ghost_eval_res = GHOST_RESULT_ERROR;
        }
    };
    break;
    case GHC_HLT: ghost_eval_res = GHOST_RESULT_STOP; break;
    default: break;
    }

    if (old_pc == mac->pc) mac->pc++;
}

void ghost_run(unsigned char ghost_index)
{
    ghost_eval_res = GHOST_RESULT_RUNNING;

    ghost *g = &ghosts[ghost_index];
    ghc_machine *mac = &g->mac;
    g->target_dir = g->dir;
    mac->pc = 0;

    while (ghost_eval_res == GHOST_RESULT_RUNNING)
    {
        /*
        if (tickcount == 1040)
            printf("-> ghost%d: %d A%d B%d C%d D%d E%d F%d G%d H%d []\n",
                    ghost_index,
                    mac->pc, mac->regs[0],
                    mac->regs[1], mac->regs[2],
                    mac->regs[3], mac->regs[4],
                    mac->regs[5], mac->regs[6],
                    mac->regs[7]);
        */
        ghost_eval_one_instr(ghost_index);
        /*
        if (tickcount == 1040)
            printf("<- ghost%d: %d A%d B%d C%d D%d E%d F%d G%d H%d []\n",
                    ghost_index,
                    mac->pc, mac->regs[0],
                    mac->regs[1], mac->regs[2],
                    mac->regs[3], mac->regs[4],
                    mac->regs[5], mac->regs[6],
                    mac->regs[7]);
        */
    }

    if (ghost_eval_res == GHOST_RESULT_ERROR)
        ghosts[ghost_index].target_dir = ghosts[ghost_index].dir;
}

int ghost_legal(ghost *g, int nfree, int dir)
{
    int x = ADVANCEX(g->x, dir);
    int y = ADVANCEY(g->y, dir);

    if (nfree > 1 && (dir == (g->dir+2) % 4))
        return 0;

    return map[x][y] != CELL_WALL;
}

#define GCC_LDC 0
#define GCC_LD 1
#define GCC_ADD 2
#define GCC_SUB 3
#define GCC_MUL 4
#define GCC_DIV 5
#define GCC_CEQ 6
#define GCC_CGT 7
#define GCC_CGTE 8
#define GCC_ATOM 9
#define GCC_CONS 10
#define GCC_CAR 11
#define GCC_CDR 12
#define GCC_SEL 13
#define GCC_JOIN 14
#define GCC_LDF 15
#define GCC_AP 16
#define GCC_RTN 17
#define GCC_DUM 18
#define GCC_RAP 19
#define GCC_TSEL 20
#define GCC_TAP 21
#define GCC_TRAP 22
#define GCC_ST 23
#define GCC_STOP 24
#define GCC_DBUG 25
#define GCC_BRK 26
#define GCC_OTHER 128
#define GCC_ERROR 129

gcc_instr read_gcc_instr(FILE *f)
{
    gcc_instr instr;
    char mnemo[10];
    char c = 0;
    unsigned char p = 0;
    unsigned char instr_type = GCC_ERROR;

    skip_space(f);

    while ( (c = fgetc(f)) != ' ' && c != '\t' && c != '\n' && c != EOF )
    {
        mnemo[p++] = toupper(c);
    }
    mnemo[p] = '\0';
    ungetc(c, f);

    if (strcmp(mnemo,"LD") == 0) { instr_type = GCC_LD; }
    if (strcmp(mnemo,"LDC") == 0) { instr_type = GCC_LDC; }
    if (strcmp(mnemo,"ADD") == 0) { instr_type = GCC_ADD; }
    if (strcmp(mnemo,"SUB") == 0) { instr_type = GCC_SUB; }
    if (strcmp(mnemo,"MUL") == 0) { instr_type = GCC_MUL; }
    if (strcmp(mnemo,"DIV") == 0) { instr_type = GCC_DIV; }
    if (strcmp(mnemo,"CEQ") == 0) { instr_type = GCC_CEQ; }
    if (strcmp(mnemo,"CGT") == 0) { instr_type = GCC_CGT; }
    if (strcmp(mnemo,"CGTE") == 0) { instr_type = GCC_CGTE; }
    if (strcmp(mnemo,"ATOM") == 0) { instr_type = GCC_ATOM; }
    if (strcmp(mnemo,"CONS") == 0) { instr_type = GCC_CONS; }
    if (strcmp(mnemo,"CAR") == 0) { instr_type = GCC_CAR; }
    if (strcmp(mnemo,"CDR") == 0) { instr_type = GCC_CDR; }
    if (strcmp(mnemo,"SEL") == 0) { instr_type = GCC_SEL; }
    if (strcmp(mnemo,"JOIN") == 0) { instr_type = GCC_JOIN; }
    if (strcmp(mnemo,"LDF") == 0) { instr_type = GCC_LDF; }
    if (strcmp(mnemo,"AP") == 0) { instr_type = GCC_AP; }
    if (strcmp(mnemo,"RTN") == 0) { instr_type = GCC_RTN; }
    if (strcmp(mnemo,"DUM") == 0) { instr_type = GCC_DUM; }
    if (strcmp(mnemo,"RAP") == 0) { instr_type = GCC_TRAP; }
    if (strcmp(mnemo,"TSEL") == 0) { instr_type = GCC_TSEL; }
    if (strcmp(mnemo,"TAP") == 0) { instr_type = GCC_TAP; }
    if (strcmp(mnemo,"TRAP") == 0) { instr_type = GCC_TRAP; }
    if (strcmp(mnemo,"ST") == 0) { instr_type = GCC_ST; }
    if (strcmp(mnemo,"STOP") == 0) { instr_type = GCC_STOP; }
    if (strcmp(mnemo,"DBUG") == 0) { instr_type = GCC_DBUG; }
    if (strcmp(mnemo,"BRK") == 0) { instr_type = GCC_BRK; }

    if (instr_type == GCC_LD ||
        instr_type == GCC_SEL ||
        instr_type == GCC_TSEL ||
        instr_type == GCC_ST)
    {
        skip_space(f);
        instr.arg1 = read_int_arg(f);
        skip_space(f);
        instr.arg2 = read_int_arg(f);
    }
 
    if (instr_type == GCC_LDC
        || instr_type == GCC_LDF
        || instr_type == GCC_AP
        || instr_type == GCC_TAP
        || instr_type == GCC_DUM
        || instr_type == GCC_RAP
        || instr_type == GCC_TRAP
        )
    {
        skip_space(f);
        instr.arg1 = read_int_arg(f);
    }

    skip_space(f);

    instr.instr = instr_type;

    return instr;
}

gcc_instr *load_gcc(char *fn)
{
    FILE *f = fopen(fn, "r");
    int codesize = 1;
    char c;
    while ( (c = fgetc(f)) != EOF )
    {
        if (c == '\n') codesize++;
    }
    fseek(f, 0, SEEK_SET);

    gcc_instr *code = (gcc_instr *)malloc(sizeof(gcc_instr) * codesize);
    int i = 0;

    while (!feof(f))
    {
        skip_space(f);

        c = fgetc(f);
        if (c == ';') { while( (c = fgetc(f)) != EOF && c != '\n' ); }
        if (c == EOF) break;
        if (c == '\n') continue;

        ungetc(c,f);

        code[i] = read_gcc_instr(f);

        if (code[i].instr == GCC_ERROR) {
            free(code);
            code = NULL;
            break;
        }

        i++;

        while( (c = fgetc(f)) != EOF && c != '\n' );
    }

    fclose(f);

    return code;
}

gcc_data gcc_make_int(int n)
{
    gcc_data g;
    g.type = GCC_DATA_INT;
    g.value = n;
    return g;
}

gcc_data gcc_make_cons(gcc_data car, gcc_data cdr)
{
    gcc_cons g;
    g.type = GCC_DATA_CONS;
    g.car = (gcc_data*) malloc(sizeof(gcc_data));
    *g.car = car;
    g.cdr = (gcc_data*) malloc(sizeof(gcc_data));
    *g.cdr = cdr;

    return *((gcc_data*) &g);
}

gcc_data gcc_make_closure(unsigned int address, gcc_frame *frame)
{
    gcc_closure c;
    c.type = GCC_DATA_CLOSURE;
    c.address = address;
    c.frame = frame;

    return *((gcc_data*)&c);
}

gcc_frame *alloc_frame(gcc_frame *parent, unsigned int size, unsigned char dummy)
{
    gcc_frame *f = (gcc_frame*)malloc(sizeof(gcc_frame));
    f->size = size;
    f->locals = (gcc_data*)malloc(sizeof(gcc_data) * size);
    f->dummy = dummy;
    f->parent = parent;
    return f;
}

gcc_data encode_world()
{
    gcc_data gcc_world;

    gcc_data gcc_map;

    gcc_map = gcc_make_int(0);
    for (int y = map_height-1; y >= 0; y--)
    {
        gcc_data line = gcc_make_int(0);
        for (int x = map_width-1; x >= 0; x--)
            line = gcc_make_cons(gcc_make_int(map[x][y]), line);
        gcc_map = gcc_make_cons(line, gcc_map);
    }

    gcc_data gcc_lambda = gcc_make_cons(
            gcc_make_int (lambdaman.vit > 0 ? lambdaman.vit - tickcount : 0),
            gcc_make_cons(
                gcc_make_cons(gcc_make_int(lambdaman.x),
                              gcc_make_int(lambdaman.y)),
                gcc_make_cons(gcc_make_int(lambdaman.dir),
                    gcc_make_cons(gcc_make_int(lambdaman.lives),
                                  gcc_make_int(lambdaman.score)))));

    gcc_data gcc_ghosts = gcc_make_int(0);
    for (int i = nghosts-1; i >= 0; i--)
    {
        ghost *g = &ghosts[i];
        gcc_data gcc_ghost = gcc_make_cons(
                gcc_make_int(g->vit),
                gcc_make_cons(
                    gcc_make_cons(gcc_make_int(g->x),
                                  gcc_make_int(g->y)),
                    gcc_make_int(g->dir)));
        gcc_ghosts = gcc_make_cons(gcc_ghost, gcc_ghosts); 
    }

    gcc_data gcc_fruit = gcc_make_int(fruit > 0 ? fruit - tickcount : 0);

    gcc_world = gcc_make_cons(
            gcc_map,
            gcc_make_cons(gcc_lambda,
                gcc_make_cons(gcc_ghosts, gcc_fruit)));

    return gcc_world;
}

#define GCC_RESULT_RUNNING 0
#define GCC_RESULT_STOP 1
#define GCC_RESULT_TAG_MISMATCH 2
#define GCC_RESULT_CONTROL_MISMATCH 3
#define GCC_RESULT_FRAME_MISMATCH 4
#define GCC_RESULT_ERROR 5

unsigned char gcc_eval_res;

gcc_control control_pop()
{
    return *(--lambdaman.mac.control_stack);
}

void control_push(gcc_control data)
{
    *(lambdaman.mac.control_stack++)  = data;
}


void data_push(gcc_data data)
{
    *(lambdaman.mac.data_stack++)  = data;
}

gcc_data data_pop()
{
    gcc_data d = *(--lambdaman.mac.data_stack);
    return d;
}


gcc_cons data_cons_pop()
{
    gcc_data d = data_pop();
    assert(d.type == GCC_DATA_CONS);
    return to_cons(d);
}

gcc_closure data_closure_pop()
{
    gcc_data d = data_pop();
    assert(d.type == GCC_DATA_CLOSURE);
    return *((gcc_closure*)(&d)); 
}

void gcc_eval_one()
{
    gcc_machine *mac = &lambdaman.mac;
    gcc_instr i = mac->code[mac->pc];
    unsigned int old_pc = mac->pc;

    /*
    printf("%d || %d %d %d\n", mac->pc, i.instr, i.arg1, i.arg2);
    fflush(stdout);
    */
    switch(i.instr) {
    case GCC_LDC:
        data_push(gcc_make_int(i.arg1));
        break;
    case GCC_LD: {
        gcc_frame *f = mac->frame;
        unsigned int n = i.arg1;
        while(n > 0) {
            f = f->parent;
            n--;
        }
        data_push(f->locals[i.arg2]);
        break;
    }
    case GCC_ADD: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value + b.value, 0});
        break;}
    case GCC_SUB: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value - b.value, 0});
        break;}
    case GCC_MUL: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value * b.value, 0});
        break;}
    case GCC_DIV: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value / b.value, 0});
        break;}
    case GCC_CEQ: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value == b.value ? 1 : 0, 0});
        break;}
    case GCC_CGT: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value > b.value ? 1 : 0, 0});
        break;}
    case GCC_CGTE: { gcc_data b = data_pop(); gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.value >= b.value ? 1 : 0, 0});
        break;}
    case GCC_ATOM: { gcc_data a = data_pop();
        data_push((gcc_data){ GCC_DATA_INT, a.type == GCC_DATA_INT ? 1 : 0, 0});
        break;}
    case GCC_CONS: { gcc_data b = data_pop();
        gcc_data a = data_pop();
        data_push(gcc_make_cons(a,b));
        break;
    }
    case GCC_CAR: { gcc_cons a = data_cons_pop();
        data_push(*a.car);
        /* free(a.car);
        free(a.cdr); TODO */
        break;}
    case GCC_CDR: { gcc_cons a = data_cons_pop();
        data_push(*a.cdr);
        break;}
    case GCC_SEL: { gcc_data x = data_pop();
        control_push((gcc_control){ GCC_CONTROL_JOIN, mac->pc+1, 0 });
        mac->pc = x.value == 0 ? i.arg2 : i.arg1;
        break; }
    case GCC_JOIN: { gcc_control c = control_pop();
        mac->pc = c.address;
        break; }
    case GCC_LDF: {
        data_push(gcc_make_closure(i.arg1, mac->frame));
        break;}
    case GCC_AP: { gcc_closure c = data_closure_pop();
        gcc_frame *f = alloc_frame(c.frame, i.arg1, 0);
        for (int k = i.arg1-1; k >= 0; k--)
        {
            f->locals[k] = data_pop();
        }
        control_push((gcc_control){ GCC_CONTROL_RETURN, mac->pc+1, mac->frame });
        mac->frame = f;
        mac->pc = c.address;
        break;
    }
    case GCC_RTN: {
        gcc_control c = control_pop();
        if (c.type == GCC_CONTROL_STOP) {
            gcc_eval_res = GCC_RESULT_STOP;
        } else if (c.type == GCC_CONTROL_RETURN) {
            mac->frame = c.frame;
            mac->pc = c.address;
        } else {
            gcc_eval_res = GCC_RESULT_CONTROL_MISMATCH;
        }
        break;}
    case GCC_DUM: {
        gcc_frame * f = alloc_frame(mac->frame, i.arg1, 1);
        mac->frame = f;
        break;}
    case GCC_RAP: { gcc_closure c = data_closure_pop();
        gcc_frame *f = c.frame;
        if (f->dummy != 1 || f->size != i.arg1 || f != mac->frame) {
            gcc_eval_res = GCC_RESULT_FRAME_MISMATCH;
            break;
        }
        for (int k = i.arg1-1; k >= 0; k--)
        {
            f->locals[k] = data_pop();
        }
        control_push((gcc_control){ GCC_CONTROL_RETURN, mac->pc+1, f->parent });
        f->dummy = 0;
        mac->frame = f;
        mac->pc = c.address;
        break;
    }
    case GCC_TSEL: { gcc_data x = data_pop();
        mac->pc = x.value == 0 ? i.arg2 : i.arg1;
        break; }
    case GCC_TAP: { gcc_closure c = data_closure_pop();
        gcc_frame *f = alloc_frame(c.frame, i.arg1, 0);
        for (int k = i.arg1-1; k >= 0; k--)
        {
            f->locals[k] = data_pop();
        }
        mac->frame = f;
        mac->pc = c.address;
        break;
    }
    case GCC_TRAP: { gcc_closure c = data_closure_pop();
        gcc_frame *f = c.frame;
        if (f->dummy != 1 || f->size != i.arg1 || f != mac->frame) {
            gcc_eval_res = GCC_RESULT_FRAME_MISMATCH;
            break;
        }
        for (int k = i.arg1-1; k >= 0; k--)
        {
            f->locals[k] = data_pop();
        }
        f->dummy = 0;
        mac->frame = f;
        mac->pc = c.address;
        break;
    }
    case GCC_ST: {
        gcc_frame *f = mac->frame;
        unsigned int n = i.arg1;
        while(n != 0) {
            f = f->parent;
            n--;
        }
        f->locals[i.arg2] = data_pop();
        break;
    }
    case GCC_DBUG:
        /*
        print_gcc_data(data_pop());
        putchar('\n');
        */
        break;
    case GCC_STOP:
        gcc_eval_res = GCC_RESULT_STOP;
    default:
        break;
    }

    if (mac->pc == old_pc) mac->pc++;
}

gcc_cons gcc_main(gcc_instr *lambdaman_code)
{
    gcc_machine *mac = &lambdaman.mac;
    mac->pc = 0;
    mac->code = lambdaman_code;
    mac->data_stack = (gcc_data*)malloc(sizeof(gcc_data) * STACK_SIZE);
    mac->data_stack_top = mac->data_stack;
    mac->control_stack = (gcc_control*)malloc(sizeof(gcc_control) * STACK_SIZE);
    mac->control_stack_top = mac->control_stack;
    mac->frame = alloc_frame(NULL, 2, 0);
    gcc_data world = encode_world();
    mac->frame->locals[0] = world;
    mac->frame->locals[1] = gcc_make_int(42);

    control_push((gcc_control){ GCC_CONTROL_STOP, 0, 0 });
    gcc_eval_res = GCC_RESULT_RUNNING;
    int cycle = 0;
    while (gcc_eval_res == GCC_RESULT_RUNNING)
    {
        gcc_eval_one();
        cycle++;
        /* printf("Instruction %d\n", cycle);
        print_gcc_machine(); */
    }

    return data_cons_pop();
}

gcc_cons gcc_step(gcc_data state, gcc_closure step_closure)
{
    gcc_machine *mac = &lambdaman.mac;
    mac->pc = step_closure.address;
    mac->frame = alloc_frame(step_closure.frame, 2, 0);
    gcc_data world = encode_world();
    mac->frame->locals[0] = state;
    mac->frame->locals[1] = world;

    control_push((gcc_control){ GCC_CONTROL_STOP, 0, 0 });

    gcc_eval_res = GCC_RESULT_RUNNING;
    int cycle = 0;
    while (gcc_eval_res == GCC_RESULT_RUNNING)
    {
        gcc_eval_one();
        cycle++;
        /*
        printf("Instruction %d\n", cycle);
        print_gcc_machine();
        */
    }

    return data_cons_pop();
}


int main(int argc, char **argv)
{
    if (sizeof(unsigned int *) != 4 || sizeof(gcc_data) != sizeof(gcc_cons) || sizeof(gcc_data) != sizeof(gcc_closure))
    {
        printf("This simulator MUST be compiled in 32 bits\n");
        return 1;
    }

    load_map(argv[1]);
    gcc_instr *lambdaman_code = load_gcc(argv[2]);
    int nghosts_codes = argc - 3;
    ghc_instr **ghosts_codes = (ghc_instr **)malloc(sizeof(ghc_instr *) * nghosts_codes);
    for (int i = 0; i < nghosts_codes; i++)
        ghosts_codes[i] = load_ghc(argv[3+i]);

    nghosts = 0;
    for (int x = 0; x < map_width; x++)
    for (int y = 0; y < map_height; y++)
        if (map[x][y] == CELL_GHOST_START) nghosts++;

    ghosts = (ghost *)malloc(sizeof(ghost) * nghosts);

    unsigned char i = 0;
    for (int y = 0; y < map_height; y++)
    for (int x = 0; x < map_width; x++) {
        if (map[x][y] == CELL_GHOST_START) {
            ghosts[i].vit = GHOST_VIT_STANDARD;
            ghosts[i].x = x;
            ghosts[i].y = y;
            ghosts[i].sx = x;
            ghosts[i].sy = y;
            ghosts[i].dir = DIRECTION_DOWN;
            ghosts[i].target_dir = 255;
            ghosts[i].tick = tick_ghosts[ i % 4 ];
            ghc_init_machine(&ghosts[i].mac, ghosts_codes[ i % nghosts_codes ]);
            i++;
        } else if (map[x][y] == CELL_LAMBDA_START) {
            lambdaman.x = x;
            lambdaman.y = y;
            lambdaman.sx = x;
            lambdaman.sy = y;
            lambdaman.dir = 0;
            lambdaman.vit = 0;
            lambdaman.lives = 3;
            lambdaman.score = 0;
            lambdaman.tick = TICK_LAMBDA_NORMAL;
        }
    }

    fruit = 0;
    unsigned int gh_score = 200;
    unsigned int level = (map_width * map_height) / 100;
    if ((map_width * map_height) % 100 != 0) level++;
    unsigned int fruit_scores[] = { 0, 100, 300, 500, 500, 700, 700, 1000, 1000, 2000, 2000, 3000, 3000, 5000 };
    unsigned int fruit_score = level <= 12 ? fruit_scores[level] : 5000;

    gcc_cons main_res = gcc_main(lambdaman_code);
    gcc_data state = *main_res.car;
    gcc_closure step_closure = *((gcc_closure*)main_res.cdr);
    free(main_res.car);
    free(main_res.cdr);

    unsigned char update = 0;

    while (1) {
        if (tickcount == lambdaman.tick) {
            gcc_cons step_res = gcc_step(state, step_closure);
            state = *step_res.car;

            unsigned int dir = step_res.cdr->value;

            int x = ADVANCEX(lambdaman.x, dir);
            int y = ADVANCEY(lambdaman.y, dir);

            lambdaman.dir = dir;

            if (map[x][y] != CELL_WALL) {
                lambdaman.x = x;
                lambdaman.y = y;
            }

            unsigned char c = map[lambdaman.x][lambdaman.y];

            if (c == CELL_PILL || c == CELL_POWERPILL
                    || (fruit > 0 && c == CELL_FRUIT))
                lambdaman.tick += TICK_LAMBDA_EATING;
            else
                lambdaman.tick += TICK_LAMBDA_NORMAL;

            update = 1;
        }

        for (unsigned char gi = 0; gi < nghosts; gi++) {
            ghost *g = &ghosts[gi];

            if (g->tick == tickcount) {
                ghost_run(gi);
            }
        }
        for (unsigned gi = 0; gi < nghosts; gi++) {
            ghost *g = &ghosts[gi];

            if (g->tick == tickcount) {
                int nfree =
                    (map[g->x-1][g->y] != CELL_WALL)
                    + (map[g->x+1][g->y] != CELL_WALL)
                    + (map[g->x][g->y-1] != CELL_WALL)
                    + (map[g->x][g->y+1] != CELL_WALL);
                
                int new_dir = DIRECTION_LEFT;

                if (ghost_legal(g, nfree, g->target_dir))
                    new_dir = g->target_dir;
                else if (ghost_legal(g, nfree, g->dir))
                    new_dir = g->dir;
                else if (ghost_legal(g, nfree, DIRECTION_UP))
                    new_dir = DIRECTION_UP;
                else if (ghost_legal(g, nfree, DIRECTION_RIGHT))
                    new_dir = DIRECTION_RIGHT;
                else if (ghost_legal(g, nfree, DIRECTION_DOWN))
                    new_dir = DIRECTION_DOWN;

                g->dir = new_dir;
                g->x = ADVANCEX(g->x, g->dir);
                g->y = ADVANCEY(g->y, g->dir);

                if (g->vit != GHOST_VIT_STANDARD)
                    g->tick += tick_ghosts_fright[ gi % 4 ];
                else
                    g->tick += tick_ghosts[ gi % 4 ];

                update = 1;
            }
        }

        if (lambdaman.vit ==  tickcount) {
            lambdaman.vit = 0;

            for (unsigned gi = 0; gi < nghosts; gi++) {
                ghost *g = &ghosts[gi];
                g->vit = GHOST_VIT_STANDARD;
            }
        }

        if (tickcount == 127 * map_width * map_height * 16)
            lambdaman.lives = 0;

        if (tickcount == 127 * 200 || tickcount == 127 * 400)
            fruit = tickcount + 80 * 127;

        if (tickcount == fruit)
            fruit = 0;

        char cell = map[lambdaman.x][lambdaman.y];

        if (cell == CELL_PILL) {
            lambdaman.score += 10;
            map[lambdaman.x][lambdaman.y] = CELL_EMPTY;
        }

        if (cell == CELL_POWERPILL) {
            lambdaman.score += 50;
            lambdaman.vit = tickcount + 127 * 20;
            gh_score = 200;
            for (unsigned gi = 0; gi < nghosts; gi++) {
                ghost *g = &ghosts[gi];
                g->vit = GHOST_VIT_FRIGHT;
                g->dir = (g->dir + 2) % 4;
            }
            map[lambdaman.x][lambdaman.y] = CELL_EMPTY;
        }

        if (fruit > 0 && cell == CELL_FRUIT) {
            lambdaman.score += fruit_score;
            fruit = 0;
        }

        for (unsigned gi = 0; gi < nghosts; gi++) {
            ghost *g = &ghosts[gi];
            if (g->x == lambdaman.x && g->y == lambdaman.y
                    && g->vit != GHOST_VIT_INVISIBLE)
            {
                if (g->vit == GHOST_VIT_STANDARD)
                {
                    lambdaman.x = lambdaman.sx;
                    lambdaman.y = lambdaman.sy;
                    for (unsigned gj = 0; gj < nghosts; gj++) {
                        ghost *og = &ghosts[gj];
                        og->x = og->sx;
                        og->y = og->sy;
                        og->dir = DIRECTION_DOWN;
                    }
                    lambdaman.lives--;
                }
                else
                {
                    g->vit = GHOST_VIT_INVISIBLE;
                    g->x = g->sx;
                    g->y = g->sy;
                    g->dir = DIRECTION_DOWN;
                    lambdaman.score += gh_score;
                    if (gh_score < 8 * 200) gh_score *= 2;
                }
            }
        }

        unsigned int npills = 0;
        for (int y = 0 ; y < map_height; y++)
            for (int x = 0; x < map_width; x++)
            {
                if (map[x][y] == CELL_PILL) npills++;
            }

        if (npills == 0) {
            lambdaman.score *= lambdaman.lives + 1;
            printf("W %d %d %d\n", lambdaman.score, lambdaman.lives, tickcount);
            break;
        }

        if (lambdaman.lives == 0) {
            printf("L %d %d %d\n", lambdaman.score, lambdaman.lives, tickcount);
            break;
        }

        if (0 && update) {
            printf("tickcount : %d score : %d\n", tickcount, lambdaman.score);
            for (int y = 0 ; y < map_height; y++)
            {
                for (int x = 0; x < map_width; x++)
                {
                    char c;
                    switch (map[x][y]) {
                    case 0: c = '#'; break;
                    case 2: c = '.'; break;
                    case 3: c = 'o'; break;
                    case 4: if (fruit > 0) {
                                c = '%'; break;
                            }
                    default:
                    case 1: c = ' '; break;
                    }
                    if (lambdaman.x == x && lambdaman.y == y)
                        c = '\\';
                    for (int gi = 0; gi < nghosts; gi++)
                    {
                        ghost *g = &ghosts[gi];
                        if (g->x == x && g->y == y)
                            c = '=';
                    }
                    putchar(c);
                }
                putchar('\n');
            }
            update = 0;
        }

        tickcount++;
        // if (tickcount > 500) break;
    }

    return 0;
}
