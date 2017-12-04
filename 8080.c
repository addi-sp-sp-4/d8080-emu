#include <stdio.h>
#include <stdlib.h>

typedef unsigned char        S8BIT;
typedef unsigned int         S16BIT;
typedef unsigned long int    S32BIT;

# define DEBUG 1

/* Registers */

/* PAIRS:
 * 
 *  BC
 *  DE
 *  HL
 */

typedef struct registers
{
    S8BIT   A       ;   /* Accumulator  */
    S8BIT   B       ;   /* Multipurpose */
    S8BIT   C       ;   /* Multipurpose */
    S8BIT   D       ;   /* Multipurpose */
    S8BIT   E       ;   /* Mutlipurpose */
    S8BIT   H       ;   /* Multipurpose */ 
    S8BIT   L       ;   /* Multipurpose */
    S16BIT  PC      ;   /* Program counter / Instruction Pointer */
    S16BIT  SP      ;   /* Stack Pointer */
    S8BIT   FLAGS   ;   /* Flags */

    /*  FLAGS:
     *  
     *  Bit 0: SIGN             
     *  Bit 1: ZERO                 
     *  Bit 2: **NONE**                 
     *  Bit 3: AUXILIARY CARRY      
     *  Bit 4: **NONE**                 
     *  Bit 5: PARITY               
     *  Bit 6: **NONE**
     *  Bit 7: CARRY        
     */

} registers;

# define FLAGS_S    0
# define FLAGS_Z    1
# define FLAGS_AC   3
# define FLAGS_P    5
# define FLAGS_CY   7



typedef struct IO
{
	S8BIT 		shift_offset ; /* Amount to shift for the shift hardware */
	S8BIT 		shift_data[2]; /* Data to shift							 */

} IO;

/* computer */
typedef struct computer
{
    registers   *reg    ; /* Registers               */
    S8BIT       *mem    ; /* Memory     16K normally */
	IO			*IO		; /* Input / Output			 */

} computer ;


/* BITWISE HELPER FUNCS */



S8BIT get_lowest(S16BIT i)
{
    return (S8BIT) i & 0xff;
}

S8BIT get_highest(S16BIT i)
{
    return (S8BIT) i >> 8;
}

S16BIT get_HL_reg(registers *r)
{
    return (S16BIT) ( (r->H << 8) | r->L);
}

S16BIT get_BC_reg(registers *r)
{
    return (S16BIT) ( (r->B << 8) | r->C );
}

S16BIT get_DE_reg(registers *r)
{
    return (S16BIT) ( (r->D << 8) | r->E );
}

void set_HL_reg(registers *r, S16BIT i)
{
    r->H = get_highest(i);
    r->L = get_lowest(i);
}

void set_BC_reg(registers *r, S16BIT i)
{
    r->B = get_highest(i);
    r->L = get_lowest(i);
}

void set_DE_reg(registers *r, S16BIT i)
{
    r->D = get_highest(i);
    r->E = get_lowest(i);
}


void set_bit(S8BIT *byte, int val, int i)
{
    *byte = (*byte & ~(1 << i)) | (val << i);
}

S8BIT get_bit(S8BIT byte, int i)
{
    return (byte & (1 << i) ) != 0;
}


void read_file_to_mem(S8BIT *mem, char *filename, S32BIT offset)
{
    FILE *f = fopen(filename, "rb");

    if(f==NULL) {printf("ERROR: could not read file \"%s\"\n", filename); exit(1);}

    fseek(f, 0L, SEEK_END);

    size_t fsize = ftell(f);

    fseek(f, 0L, SEEK_SET);

    S8BIT *b = &mem[offset];
    fread(b, fsize, 1, f);
    fclose(f);
}

/* How much the PC should increment for each instruction */
/* This often is the instruction size */
/* in the instructions that alter the PC reg manually, i call return, so the value in here does not get used */
S8BIT instruction_sizes[0x100] = 
{
    1, 3, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,     /* 0x00 - 0x0f */
    1, 3, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1,     /* 0x10 - 0x1f */
    1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 2, 1,     /* 0x20 - 0x2f */
    1, 3, 3, 1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 2, 1,     /* 0x30 - 0x3f */

    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x40 - 0x4f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x50 - 0x5f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x60 - 0x6f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x70 - 0x7f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x80 - 0x8f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0x90 - 0x9f */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0xa0 - 0xaf */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,     /* 0xb0 - 0xbf */

    1, 1, 3, 3, 3, 1, 2, 1, 1, 1, 3, 1, 3, 3, 2, 1,     /* 0xc0 - 0xcf */
    1, 1, 3, 2, 3, 1, 2, 1, 1, 1, 3, 2, 3, 1, 2, 1,     /* 0xd0 - 0xdf */
    1, 1, 3, 1, 3, 1, 2, 1, 1, 1, 3, 1, 3, 1, 2, 1,     /* 0xe0 - 0xef */
    1, 1, 3, 1, 3, 1, 2, 1, 1, 1, 3, 1, 3, 1, 2, 1      /* 0xf0 - 0xff */
} ;


S8BIT parity(S8BIT i)
{
    S8BIT r = 0;

    for(int j = 0; j < 8; j++) r += (i >> j) & 0x01;
    return !(r & 1);
}


void set_SZP_flags(computer *c, S8BIT s)
{
    
    set_bit(&c->reg->FLAGS, get_bit(s, 7),      FLAGS_S);
    set_bit(&c->reg->FLAGS, (s == 0),           FLAGS_Z);
    set_bit(&c->reg->FLAGS, (parity(s)),        FLAGS_P);

}


/* crafts an address from two bytes. Do account for little-endian! */
S16BIT toaddr(S8BIT two, S8BIT one)
{
    return (two << 8) | one;
}

void swap(S8BIT *lhand, S8BIT *rhand)
{
	*lhand ^= *rhand;
	*rhand ^= *lhand;
	*lhand ^= *rhand;
}

/* INSTRUCTIONS TOO BIG FOR SINGLE LINES */
void ins_RLC(computer *c)
{
    S8BIT p = c->reg->A;

    c->reg->A <<= 1;

    set_bit(&c->reg->A,     get_bit(p, 7), 0); /* set bit 0 of a to previous bit 7*/

    set_bit(&c->reg->FLAGS, get_bit(p, 7), FLAGS_CY); /* set carry bit to previous bit 7 */
}

/* DAD B / DAD D / DAD H / DAD SP */
void ins_DADX(computer *c, S16BIT rhand)
{
    S32BIT r = get_HL_reg(c->reg) + rhand;

    set_HL_reg(c->reg, (S16BIT)r );

    set_bit(&c->reg->FLAGS, (r >> 16) > 0, FLAGS_CY); /*  carry bit if carry occurred */
}

/* Add rhand to lhand */
S8BIT ins_ADDX(computer *c, S8BIT *lhand, S8BIT rhand)
{
    S16BIT r = *lhand + rhand;
    
    *lhand += rhand;
    
    set_bit(&c->reg->FLAGS, (r >> 8) > 0, FLAGS_CY); /* carry bit */

    return *lhand;
}

/* Add rhand + CY flag to lhand */
S8BIT ins_ADCX(computer *c, S8BIT *lhand, S8BIT rhand)
{
    S16BIT r = *lhand + rhand + get_bit(c->reg->FLAGS, FLAGS_CY);

    *lhand += (rhand + get_bit(c->reg->FLAGS, FLAGS_CY));

    set_bit(&c->reg->FLAGS, (r >> 8) > 0, FLAGS_CY); /* carry bit */

    return *lhand;


}

/* bitwise AND rhand with lhand and store in lhand */
S8BIT ins_ANAX(computer *c, S8BIT *lhand, S8BIT rhand)
{
    S16BIT r = *lhand & rhand;

    *lhand &= rhand;

    set_bit(&c->reg->FLAGS, (r >> 8) > 0, FLAGS_CY); /* carry bit */
    
    return *lhand;
}

/* bitwise XOR rhand with lhand and store in lhand */
S8BIT ins_XRAX(computer *c, S8BIT *lhand, S8BIT rhand)
{
    S16BIT r = *lhand ^ rhand;

    *lhand ^= rhand;

    set_bit(&c->reg->FLAGS, (r >> 8) > 0, FLAGS_CY); /* carry bit */

    return *lhand;
}

/* bitwise OR rhand with lhand and store in lhand */
S8BIT ins_ORAX(computer *c, S8BIT *lhand, S8BIT rhand)
{
    S16BIT r = *lhand | rhand;

    *lhand |= rhand;

    set_bit(&c->reg->FLAGS, (r >> 8) > 0, FLAGS_CY); /* carry bit */

    return *lhand;
}

/* compare */
S8BIT ins_CMPX(computer *c, S8BIT lhand, S8BIT rhand)
{
    set_bit(&c->reg->FLAGS, (lhand < rhand), FLAGS_CY );

    S8BIT r = (0x100 - rhand + lhand) % 0x100;

    return r;
}

void ins_RRC(computer *c)
{
    S8BIT p = c->reg->A;
    
    c->reg->A >>= 1;
    
    set_bit(&c->reg->A, (p & 0x01) == 0x01, 7 ); /* set bit 7 of A to previous bit 0 */
    
    set_bit(&c->reg->FLAGS, (p & 0x01) == 0x01, FLAGS_CY ); /* Set carry bit (bit 3) to previous bit 0 */

}

void ins_RAL(computer *c)
{
    S8BIT p = c->reg->A;
    
    c->reg->A <<= 1;
    
    set_bit(&c->reg->A, get_bit(c->reg->FLAGS, FLAGS_CY), 0); /* Set bit 0 of A to previous carry bit */
    
    set_bit(&c->reg->FLAGS, get_bit(p, 7), FLAGS_CY); /* Set carry bit to previous bit 7 */

}


void ins_RAR(computer *c)
{
    S8BIT p = c->reg->A;

    c->reg->A >>= 1;

    set_bit(&c->reg->A, (p & 0x80) == 0x80, 7); /* Set bit 7 of a to previous bit 7 */

    set_bit(&c->reg->FLAGS, get_bit(p, 0), FLAGS_CY); /* Set carry bit to previous bit 0 */
}

void ins_SHLD(computer *c, S8BIT *opcode)
{
    c->mem[toaddr(2[opcode], 1[opcode])] = c->reg->L;
    c->mem[toaddr(2[opcode], 1[opcode]) + 1] = c->reg->H;
}


void ins_LHLD(computer *c, S8BIT *opcode)
{
    
    S16BIT addr = toaddr(2[opcode], 1[opcode]);

    c->reg->L = c->mem[addr];
    c->reg->H = c->mem[addr + 1];
}

void ins_RET(computer *c)
{
    c->reg->PC = 0;

    c->reg->PC | (c->mem[c->reg->SP] << 8) | c->mem[c->reg->SP];

    c->reg->SP += 2;
}

void ins_POPX(computer *c, S8BIT *rhand, S8BIT *lhand)
{
    *lhand = c->mem[c->reg->SP];

    *rhand = c->mem[c->reg->SP + 1];

    c->reg->SP += 2;
}

void ins_JMP(computer *c, S8BIT *opcode)
{

    c->reg->PC = toaddr(2[opcode], 1[opcode]);
}

void ins_CALL(computer *c, S16BIT address)
{
    c->mem[c->reg->SP - 1] = get_highest(c->reg->PC);
    c->mem[c->reg->SP - 2] = get_lowest(c->reg->PC);
    c->reg->SP += 2;
    c->reg->PC = address;
}

void ins_PUSHX(computer *c, S8BIT lhand, S8BIT rhand)
{
    c->mem[c->reg->SP - 2] = lhand;
    c->mem[c->reg->SP - 1] = rhand;
    c->reg->SP -= 2;
}

void ins_OUT(computer *c, S8BIT port)
{
	
	//switch(port)
	//{
		/* Shift register offset */	
	//	case 2: 
	//		c->IO->shift_offset = (c->reg->A & 0x07);
	//	   	break;
	//	
	//	/* Byte to shift */
	//	case 4:
	//	   	c->IO->shift_data[0] = c->IO->shift_data[1];
	//		c->IO->shift_data[1] = c->reg->A;
	//		break; 

//	}

}

void ins_IN(computer *c, S8BIT port)
{
	//switch(port)
	//{
	//
	//	case 3:
	//		S16BIT v = (c->IO->shift_data[1] << 8) | c->IO->shift_data[0];
	//		c->reg->A = ((v >> (8 - c->IO->shift_offset)) & 0xff);
	//	   	break;
	//		

//	}
}

int emulate_instruction(computer *c)
{

    S8BIT *opcode = &c->mem[c->reg->PC];

    
    #ifdef DEBUG
        printf("%#06x: %#04x\n", c->reg->PC, *opcode);
    #endif
    
    switch(*opcode)
    {
            /* Instruction 0x00 - 0x0f */

            case 0x00:  /* NOP */                                                                               break;  /* NOP          */              
            case 0x01:  c->reg->B = 2[opcode];                          c->reg->C = 1[opcode];                  break;  /* LXI B, D16   */
            case 0x02:  c->mem[ get_BC_reg(c->reg) ] = c->reg->A;                                               break;  /* STAX B       */
            case 0x03:  if(++c->reg->B == 0) c->reg->C++;                                                       break;  /* INX B        */
            case 0x04:  set_SZP_flags(c, ++c->reg->B);                                                          break;  /* INR B        */
            case 0x05:  set_SZP_flags(c, --c->reg->B);                                                          break;  /* DCR B        */
            case 0x06:  c->reg->B = 1[opcode];                                                                  break;  /* MVI B, D8    */
            case 0x07:  ins_RLC(c);                                                                             break;  /* RLC          */
            case 0x08:  /* Interrupt */                                                                         break;  /* INTERRUPT    */
            case 0x09:  ins_DADX(c, get_BC_reg(c->reg));                                                        break;  /* DAD B        */
            case 0x0a:  c->reg->A = c->mem[get_BC_reg(c->reg)];                                                 break;  /* LDAX B       */
            case 0x0b:  set_BC_reg(c->reg, get_BC_reg(c->reg) - 1);                                             break;  /* DCX B        */
            case 0x0c:  set_SZP_flags(c, ++c->reg->C);                                                          break;  /* INR C        */
            case 0x0d:  set_SZP_flags(c, --c->reg->C);                                                          break;  /* DCR C        */
            case 0x0e:  c->reg->C += 1[opcode];                                                                 break;  /* MVI C, D8    */
            case 0x0f:  ins_RRC(c);                                                                             break;  /* RRC          */
            
            /*-----------------------------------------------------------------------------------------------------------------------------*/
            
            /* Instruction 0x10 - 0x1f */

            case 0x10: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0x11: c->reg->D = 2[opcode];                           c->reg->E = 1[opcode];                  break;  /* LXI D, D16   */
            case 0x12: c->mem[get_DE_reg(c->reg)] = c->reg->A;                                                  break;  /* STAX D       */
            case 0x13: set_DE_reg(c->reg, get_DE_reg(c->reg) + 1);                                              break;  /* INX D        */
            case 0x14: set_SZP_flags(c, ++c->reg->D);                                                           break;  /* INR D        */
            case 0x15: set_SZP_flags(c, --c->reg->D);                                                           break;  /* DCR D        */
            case 0x16: c->reg->D = 1[opcode];                                                                   break;  /* MVI D, D8    */
            case 0x17: ins_RAL(c);                                                                              break;  /* RAL          */
            case 0x18: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0x19: ins_DADX(c, get_DE_reg(c->reg));                                                         break;  /* DAD D        */
            case 0x1a: c->reg->A = c->mem[get_DE_reg(c->reg)];                                                  break;  /* LDAX D       */
            case 0x1b: set_DE_reg(c->reg, get_DE_reg(c->reg) - 1);                                              break;  /* DCX B        */
            case 0x1c: set_SZP_flags(c, ++c->reg->E);                                                           break;  /* INR E        */
            case 0x1d: set_SZP_flags(c, --c->reg->E);                                                           break;  /* DCR E        */
            case 0x1e: c->reg->E = 1[opcode];                                                                   break;  /* MVI E,D8     */
            case 0x1f: ins_RAR(c);                                                                              break;  /* RAR          */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /*Instruction 0x20-0x2f */

            case 0x20: /* TODO implement */                                                                     break;  /* RIM          */
            case 0x21: c->reg->H = 2[opcode];                           c->reg->L = 1[opcode];                  break;  /* LXI  H,D16   */
            case 0x22: ins_SHLD(c, opcode);                                                                     break;  /* SHLD addr    */
            case 0x23: set_HL_reg(c->reg, get_HL_reg(c->reg) + 1);                                              break;  /* INX H        */
            case 0x24: set_SZP_flags(c, ++c->reg->H);                                                           break;  /* INR H        */
            case 0x25: set_SZP_flags(c, --c->reg->H);                                                           break;  /* DCR H        */
            case 0x26: c->reg->H = 1[opcode];                                                                   break;  /* MVI H, D8    */
            case 0x27: /* TODO implement */                                                                     break;  /* DAA          */
            case 0x28: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0x29: ins_DADX(c, get_HL_reg(c->reg));                                                         break;  /* DAD H        */
            case 0x2a: ins_LHLD(c, opcode);                                                                     break;  /* LHLD         */
            case 0x2b: set_HL_reg(c->reg, get_HL_reg(c->reg) - 1);                                              break;  /* DCX H        */
            case 0x2c: set_SZP_flags(c, ++c->reg->L);                                                           break;  /* INR L        */
            case 0x2d: set_SZP_flags(c, --c->reg->L);                                                           break;  /* DCR L        */
            case 0x2e: c->reg->L = 1[opcode];                                                                   break;  /* MVI L, D8    */
            case 0x2f: c->reg->A = ~c->reg->A;                                                                  break;  /* CMA          */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x30-0x3f */

            case 0x30: /* TODO implement */                                                                     break;  /* SIM          */
            case 0x31: c->reg->SP = toaddr(2[opcode], 1[opcode]);                                               break;  /* LXI SP, D16  */
            case 0x32: c->mem[toaddr(2[opcode], 1[opcode])] = c->reg->A;                                        break;  /* STA addr     */
            case 0x33: c->reg->SP++;                                                                            break;  /* INX SP       */
            case 0x34: set_SZP_flags(c, ++c->mem[get_HL_reg(c->reg)]);                                          break;  /* INR M        */
            case 0x35: set_SZP_flags(c, --c->mem[get_HL_reg(c->reg)]);                                          break;  /* DCR M        */
            case 0x36: c->mem[get_HL_reg(c->reg)] = 1[opcode];                                                  break;  /* MVI M, D8    */
            case 0x37: set_bit(&c->reg->FLAGS, 1, FLAGS_CY);                                                    break;  /* STC          */
            case 0x38: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0x39: ins_DADX(c, c->reg->SP);                                                                 break;  /* DAD SP       */
            case 0x3a: c->reg->A = c->mem[toaddr(2[opcode], 1[opcode])];                                        break;  /* LDA  addr    */
            case 0x3b: c->reg->SP--;                                                                            break;  /* DCX SP       */
            case 0x3c: set_SZP_flags(c, ++c->reg->A);                                                           break;  /* INR A        */
            case 0x3d: set_SZP_flags(c, --c->reg->A);                                                           break;  /* DCR A        */
            case 0x3e: c->reg->A = 1[opcode];                                                                   break;  /* MVI A, D8    */
            case 0x3f: set_bit(&c->reg->FLAGS, get_bit(c->reg->FLAGS, FLAGS_CY) == 0, FLAGS_CY);                break;  /* CMC          */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x40-0x4f */

            case 0x40: c->reg->B = c->reg->B;                                                                   break;  /* MOV B, B     */
            case 0x41: c->reg->B = c->reg->C;                                                                   break;  /* MOV B, C     */
            case 0x42: c->reg->B = c->reg->D;                                                                   break;  /* MOV B, D     */
            case 0x43: c->reg->B = c->reg->E;                                                                   break;  /* MOV B, E     */
            case 0x44: c->reg->B = c->reg->H;                                                                   break;  /* MOV B, H     */
            case 0x45: c->reg->B = c->reg->L;                                                                   break;  /* MOV B, L     */
            case 0x46: c->reg->B = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV B, M     */
            case 0x47: c->reg->B = c->reg->A;                                                                   break;  /* MOV B, A     */
            case 0x48: c->reg->C = c->reg->B;                                                                   break;  /* MOV C, B     */
            case 0x49: c->reg->C = c->reg->C;                                                                   break;  /* MOV C, C     */
            case 0x4a: c->reg->C = c->reg->D;                                                                   break;  /* MOV C, D     */
            case 0x4b: c->reg->C = c->reg->E;                                                                   break;  /* MOV C, E     */
            case 0x4c: c->reg->C = c->reg->H;                                                                   break;  /* MOV C, H     */
            case 0x4d: c->reg->C = c->reg->L;                                                                   break;  /* MOV C, L     */
            case 0x4e: c->reg->C = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV C, M     */
            case 0x4f: c->reg->C = c->reg->A;                                                                   break;  /* MOV C, A     */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x50-0x5f */
            
            case 0x50: c->reg->D = c->reg->B;                                                                   break;  /* MOV D, B     */
            case 0x51: c->reg->D = c->reg->C;                                                                   break;  /* MOV D, C     */
            case 0x52: c->reg->D = c->reg->D;                                                                   break;  /* MOV D, D     */
            case 0x53: c->reg->D = c->reg->E;                                                                   break;  /* MOV D, E     */
            case 0x54: c->reg->D = c->reg->H;                                                                   break;  /* MOV D, H     */
            case 0x55: c->reg->D = c->reg->L;                                                                   break;  /* MOV D, L     */
            case 0x56: c->reg->D = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV D, M     */
            case 0x57: c->reg->D = c->reg->A;                                                                   break;  /* MOV D, A     */
            case 0x58: c->reg->E = c->reg->B;                                                                   break;  /* MOV E, B     */
            case 0x59: c->reg->E = c->reg->C;                                                                   break;  /* MOV E, C     */
            case 0x5a: c->reg->E = c->reg->D;                                                                   break;  /* MOV E, D     */
            case 0x5b: c->reg->E = c->reg->E;                                                                   break;  /* MOV E, E     */
            case 0x5c: c->reg->E = c->reg->H;                                                                   break;  /* MOV E, H     */
            case 0x5d: c->reg->E = c->reg->L;                                                                   break;  /* MOV E, L     */
            case 0x5e: c->reg->E = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV E, M     */
            case 0x5f: c->reg->E = c->reg->A;                                                                   break;  /* MOV E, A     */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x60-0x6f */

            case 0x60: c->reg->H = c->reg->B;                                                                   break;  /* MOV H, B     */
            case 0x61: c->reg->H = c->reg->C;                                                                   break;  /* MOV H, C     */
            case 0x62: c->reg->H = c->reg->D;                                                                   break;  /* MOV H, D     */
            case 0x63: c->reg->H = c->reg->E;                                                                   break;  /* MOV H, E     */
            case 0x64: c->reg->H = c->reg->H;                                                                   break;  /* MOV H, H     */
            case 0x65: c->reg->H = c->reg->L;                                                                   break;  /* MOV H, L     */
            case 0x66: c->reg->H = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV H, M     */
            case 0x67: c->reg->H = c->reg->A;                                                                   break;  /* MOV H, A     */
            case 0x68: c->reg->L = c->reg->B;                                                                   break;  /* MOV L, B     */
            case 0x69: c->reg->L = c->reg->C;                                                                   break;  /* MOV L, C     */
            case 0x6a: c->reg->L = c->reg->D;                                                                   break;  /* MOV L, D     */
            case 0x6b: c->reg->L = c->reg->E;                                                                   break;  /* MOV L, E     */
            case 0x6c: c->reg->L = c->reg->H;                                                                   break;  /* MOV L, H     */
            case 0x6d: c->reg->L = c->reg->L;                                                                   break;  /* MOV L, L     */
            case 0x6e: c->reg->L = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV L, M     */
            case 0x6f: c->reg->L = c->reg->A;                                                                   break;  /* MOV L, A     */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x70-0x7f */
            
            case 0x70: c->mem[get_HL_reg(c->reg)] = c->reg->B;                                                  break;  /* MOV M, B     */  
            case 0x71: c->mem[get_HL_reg(c->reg)] = c->reg->C;                                                  break;  /* MOV M, C     */
            case 0x72: c->mem[get_HL_reg(c->reg)] = c->reg->D;                                                  break;  /* MOV M, D     */
            case 0x73: c->mem[get_HL_reg(c->reg)] = c->reg->E;                                                  break;  /* MOV M, E     */
            case 0x74: c->mem[get_HL_reg(c->reg)] = c->reg->H;                                                  break;  /* MOV M, H     */
            case 0x75: c->mem[get_HL_reg(c->reg)] = c->reg->L;                                                  break;  /* MOV M, L     */
            case 0x76: return 1;                                                                                break;  /* HLT          */
            case 0x77: c->mem[get_HL_reg(c->reg)] = c->reg->A;                                                  break;  /* MOV M, A     */
            case 0x78: c->reg->A = c->reg->B;                                                                   break;  /* MOV A, B     */
            case 0x79: c->reg->A = c->reg->C;                                                                   break;  /* MOV A, C     */
            case 0x7a: c->reg->A = c->reg->D;                                                                   break;  /* MOV A, D     */
            case 0x7b: c->reg->A = c->reg->E;                                                                   break;  /* MOV A, E     */
            case 0x7c: c->reg->A = c->reg->H;                                                                   break;  /* MOV A, H     */
            case 0x7d: c->reg->A = c->reg->L;                                                                   break;  /* MOV A, L     */
            case 0x7e: c->reg->A = c->mem[get_HL_reg(c->reg)];                                                  break;  /* MOV A, M     */
            case 0x7f: c->reg->A = c->reg->A;                                                                   break;  /* MOV A, A     */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0x80-0x8f */

            case 0x80: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->B));                                    break;  /* ADD B        */
            case 0x81: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->C));                                    break;  /* ADD C        */
            case 0x82: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->D));                                    break;  /* ADD D        */
            case 0x83: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->E));                                    break;  /* ADD E        */
            case 0x84: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->H));                                    break;  /* ADD H        */
            case 0x85: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->L));                                    break;  /* ADD L        */
            case 0x86: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* ADD M        */
            case 0x87: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, c->reg->A));                                    break;  /* ADD A        */
            case 0x88: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->B));                                    break;  /* ADC B        */  
            case 0x89: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->C));                                    break;  /* ADC C        */
            case 0x8a: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->D));                                    break;  /* ADC D        */
            case 0x8b: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->E));                                    break;  /* ADC E        */
            case 0x8c: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->H));                                    break;  /* ADC H        */
            case 0x8d: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->L));                                    break;  /* ADC L        */
            case 0x8e: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* ADC M        */
            case 0x8f: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, c->reg->A));                                    break;  /* ADC A        */

            /*-----------------------------------------------------------------------------------------------------------------------------*/
            
            /* Instruction 0x90-0x9f */

            case 0x90: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->B));                                   break;  /* SUB B        */
            case 0x91: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->C));                                   break;  /* SUB C        */
            case 0x92: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->D));                                   break;  /* SUB D        */
            case 0x93: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->E));                                   break;  /* SUB E        */
            case 0x94: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->H));                                   break;  /* SUB H        */
            case 0x95: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->L));                                   break;  /* SUB L        */
            case 0x96: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->mem[get_HL_reg(c->reg)]));                  break;  /* SUB M        */
            case 0x97: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -c->reg->A));                                   break;  /* SUB A        */
            case 0x98: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->B));                                   break;  /* SBB B        */  
            case 0x99: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->C));                                   break;  /* SBB C        */
            case 0x9a: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->D));                                   break;  /* SBB D        */
            case 0x9b: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->E));                                   break;  /* SBB E        */
            case 0x9c: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->H));                                   break;  /* SBB H        */
            case 0x9d: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->L));                                   break;  /* SBB L        */
            case 0x9e: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->mem[get_HL_reg(c->reg)]));                  break;  /* SBB M        */
            case 0x9f: set_SZP_flags(c, ins_ADCX(c, &c->reg->A, -c->reg->A));                                   break;  /* SBB A        */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xa0-0xaf */

            case 0xa0: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->B));                                    break;  /* ANA B        */  
            case 0xa1: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->C));                                    break;  /* ANA C        */
            case 0xa2: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->D));                                    break;  /* ANA D        */
            case 0xa3: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->E));                                    break;  /* ANA E        */
            case 0xa4: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->H));                                    break;  /* ANA F        */
            case 0xa5: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->L));                                    break;  /* ANA L        */
            case 0xa6: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* ANA M        */
            case 0xa7: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, c->reg->A));                                    break;  /* ANA A        */
            case 0xa8: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->B));                                    break;  /* XRA B        */  
            case 0xa9: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->C));                                    break;  /* XRA C        */
            case 0xaa: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->D));                                    break;  /* XRA D        */
            case 0xab: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->E));                                    break;  /* XRA E        */
            case 0xac: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->H));                                    break;  /* XRA F        */
            case 0xad: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->L));                                    break;  /* XRA L        */
            case 0xae: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* XRA M        */
            case 0xaf: set_SZP_flags(c, ins_XRAX(c, &c->reg->A, c->reg->A));                                    break;  /* XRA A        */
            
            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xb0-0xbf */

                
            case 0xb0: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->B));                                    break;  /* ORA B        */  
            case 0xb1: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->C));                                    break;  /* ORA C        */
            case 0xb2: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->D));                                    break;  /* ORA D        */
            case 0xb3: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->E));                                    break;  /* ORA E        */
            case 0xb4: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->H));                                    break;  /* ORA F        */
            case 0xb5: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->L));                                    break;  /* ORA L        */
            case 0xb6: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* ORA M        */
            case 0xb7: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, c->reg->A));                                    break;  /* ORA A        */
            case 0xb8: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->B));                                    break;  /* CMP B        */  
            case 0xb9: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->C));                                    break;  /* CMP C        */
            case 0xba: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->D));                                    break;  /* CMP D        */
            case 0xbb: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->E));                                    break;  /* CMP E        */
            case 0xbc: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->H));                                    break;  /* CMP F        */
            case 0xbd: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->L));                                    break;  /* CMP L        */
            case 0xbe: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->mem[get_HL_reg(c->reg)]));                   break;  /* CMP M        */
            case 0xbf: set_SZP_flags(c, ins_CMPX(c,  c->reg->A, c->reg->A));                                    break;  /* CMP A        */
           
            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xc0-0xcf */

            case 0xc0: if (!get_bit(c->reg->FLAGS, FLAGS_Z) ) { ins_RET(c);                           return 0;}break;  /* RNZ          */
            case 0xc1: ins_POPX(c, &c->reg->B, &c->reg->C);                                                     break;  /* POP B        */
            case 0xc2: if(!get_bit(c->reg->FLAGS, FLAGS_Z))   { ins_JMP(c, opcode);                   return 0;}break;  /* JNZ          */
            case 0xc3: ins_JMP(c, opcode);                                                            return 0; break;  /* JMP          */
            case 0xc4: if(!get_bit(c->reg->FLAGS,FLAGS_Z)){ins_CALL(c,toaddr(opcode[2],opcode[1]));   return 0;}break;  /* CNZ addr     */ /* sorry for space stripping */
            case 0xc5: ins_PUSHX(c, c->reg->C, c->reg->B);                                                      break;  /* PUSH B       */ 
            case 0xc6: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, opcode[1]));                                    break;  /* ADI D8       */
            case 0xc7: ins_CALL(c, (S16BIT) 0x00);                                                    return 0; break;  /* RST 0        */
            case 0xc8: if(get_bit(c->reg->FLAGS, FLAGS_Z) )   { ins_RET(c);                           return 0;}break;  /* RZ           */
            case 0xc9: ins_RET(c);                                                                    return 0; break;  /* RET          */
            case 0xca: if(get_bit(c->reg->FLAGS, FLAGS_Z) )   { ins_JMP (c, opcode);                  return 0;}break;  /* JZ           */
            case 0xcb: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0xcc: if(get_bit(c->reg->FLAGS, FLAGS_Z)){ins_CALL(c,toaddr(opcode[2],opcode[1]));   return 0;}break;  /* CZ           */
            case 0xcd: ins_CALL(c, toaddr(opcode[2], opcode[1]));                                     return 0; break;  /* CALL         */
            case 0xce: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, opcode[1] + get_bit(c->reg->FLAGS, FLAGS_CY))); break;  /* ACI D8       */
            case 0xcf: ins_CALL(c, (S16BIT) 0x08);                                                    return 0; break;  /* RST 1        */

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xd0-0xdf */

            case 0xd0: if(!get_bit(c->reg->FLAGS, FLAGS_CY)) { ins_RET(c);                            return 0;}break;  /* RNC          */
            case 0xd1: ins_POPX(c, &c->reg->E, &c->reg->D);                                                     break;  /* POP D        */
            case 0xd2: if(!get_bit(c->reg->FLAGS, FLAGS_CY)) { ins_JMP(c, opcode); }                            break;  /* JNC addr     */
            case 0xd3: ins_OUT(c, opcode[1]);													                break;  /* OUT D8       */
            case 0xd4: if(!get_bit(c->reg->FLAGS,FLAGS_CY)){ins_CALL(c,toaddr(opcode[2],opcode[1]));  return 0;}break;  /* CNC addr     */
            case 0xd5: ins_PUSHX(c, c->reg->E, c->reg->D);                                                      break;  /* PUSH D       */
            case 0xd6: set_SZP_flags(c, ins_ADDX(c, &c->reg->A, -opcode[1]));                                   break;  /* SUI D8       */
            case 0xd7: ins_CALL(c, (S16BIT) 0x10);                                                    return 0; break;  /* RST 2        */
            case 0xd8: if(get_bit(c->reg->FLAGS, FLAGS_CY)) { ins_RET(c);                             return 0;}break;  /* RC           */
            case 0xd9: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0xda: if(get_bit(c->reg->FLAGS, FLAGS_CY)) { ins_JMP(c, opcode);                     return 0;}break;  /* JC addr      */
            case 0xdb: ins_IN(c, opcode[1]);										                            break;  /* IN D8        */
            case 0xdc: if(get_bit(c->reg->FLAGS,FLAGS_CY)){ins_CALL(c,toaddr(opcode[2],opcode[1]));   return 0;}break;  /* CC addr      */
            case 0xdd: /* Interrupt */                                                                          break;  /* Interrupt    */
            case 0xde: set_SZP_flags(c,ins_ADDX(c,&c->reg->A,-1*(opcode[1]+get_bit(c->reg->FLAGS,FLAGS_CY))));  break;  /* SBI D8       */
            case 0xdf: ins_CALL(c, (S16BIT) 0x18);                                                    return 0; break;  /* RST 3        */  

            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xe0-0xef */

            case 0xe0: if(!get_bit(c->reg->FLAGS, FLAGS_P)) {ins_RET(c);                              return 0;}break;  /* RPO          */
            case 0xe1: ins_POPX(c, &c->reg->H, &c->reg->H);                                                     break;  /* POP H        */
            case 0xe2: if(!get_bit(c->reg->FLAGS, FLAGS_P)) {ins_JMP(c, opcode);                      return 0;}break;  /* JPO          */
            case 0xe3: swap(&c->reg->L, &c->mem[c->reg->SP]); swap(&c->reg->H, &c->mem[c->reg->SP + 1]);        break;  /* XTHL         */
            case 0xe4: if(!get_bit(c->reg->FLAGS, FLAGS_P)) {ins_CALL(c,toaddr(opcode[2],opcode[1])); return 0;}break;  /* CPO          */
            case 0xe5: ins_PUSHX(c, c->reg->L, c->reg->H);                                                      break;  /* PUSH H       */
            case 0xe6: set_SZP_flags(c, ins_ANAX(c, &c->reg->A, opcode[1]));                                    break;  /* ANI D8       */
            case 0xe7: ins_CALL(c, (S16BIT) 0x20);                                                    return 0; break;  /* RST 4        */
            case 0xe8: if(get_bit(c->reg->FLAGS, FLAGS_P)) {ins_RET(c);                               return 0;}break;  /* RPE          */
            case 0xe9: c->reg->PC=0; c->reg->PC |= (c->reg->H << 8); c->reg->PC |= c->reg->L;         return 0; break;  /* PCHL         */
            case 0xea: if(get_bit(c->reg->FLAGS, FLAGS_P)) {ins_JMP(c, opcode);                       return 0;}break;  /* JPE addr     */
            case 0xeb: swap(&c->reg->H, &c->reg->D); swap(&c->reg->L, &c->reg->E);                              break;  /* XCHG         */
            case 0xec: if(get_bit(c->reg->FLAGS,FLAGS_P)){ins_CALL(c,toaddr(opcode[2],opcode[1]));    return 0;}break;  /* CPE addr     */
            case 0xed: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0xee: set_SZP_flags(c, ins_XRAX(c,&c->reg->A, opcode[1]));                                     break;  /* XRI D8       */
            case 0xef: ins_CALL(c, (S16BIT) 0x28);                                                    return 0; break;  /* RST 5        */
            
            /*-----------------------------------------------------------------------------------------------------------------------------*/

            /* Instruction 0xf0-0xff */
            
            case 0xf0: if(!get_bit(c->reg->FLAGS, FLAGS_S)) {ins_RET(c);                              return 0;}break;  /* RP           */
            case 0xf1: ins_POPX(c, &c->reg->FLAGS, &c->reg->A);                                                 break;  /* POP PSW      */
            case 0xf2: if(!get_bit(c->reg->FLAGS, FLAGS_S)) {ins_JMP(c, opcode);                      return 0;}break;  /* JP addr      */
            case 0xf3: /* Disable interrupts */                                                                 break;  /* DI           */
            case 0xf4: if(!get_bit(c->reg->FLAGS,FLAGS_S)){ins_CALL(c,toaddr(opcode[2],opcode[1]));   return 0;}break;  /* CP addr      */
            case 0xf5: ins_PUSHX(c, c->reg->FLAGS, c->reg->A);                                                  break;  /* PUSH PSW     */
            case 0xf6: set_SZP_flags(c, ins_ORAX(c, &c->reg->A, opcode[1]));                                    break;  /* ORI D8       */
            case 0xf7: ins_CALL(c, (S16BIT) 0x30);                                                    return 0; break;  /* RST 6        */
            case 0xf8: if(get_bit(c->reg->FLAGS, FLAGS_S)) {ins_RET(c);                               return 0;}break;  /* RM           */
            case 0xf9: c->reg->SP = get_HL_reg(c->reg);                                                         break;  /* SPHL         */
            case 0xfa: if(get_bit(c->reg->FLAGS, FLAGS_S)) {ins_JMP(c, opcode);                       return 0;}break;  /* JM addr      */
            case 0xfb: /* Enable interrupts */                                                                  break;  /* EI           */
            case 0xfc: if(get_bit(c->reg->FLAGS,FLAGS_S)){ins_CALL(c,toaddr(opcode[2],opcode[1]));    return 0;}break;  /* CM addr      */
            case 0xfd: /* Interrupt */                                                                          break;  /* INTERRUPT    */
            case 0xfe: set_SZP_flags(c, ins_CMPX(c, c->reg->A, opcode[1]));                                     break;  /* CPI D8       */
            case 0xff: ins_CALL(c, (S16BIT) 0x36);                                                    return 0; break;  /* RST 7        */



            default: printf("Instruction %#04x not implemented yet.\n", *opcode); return 1;
    }

    c->reg->PC += instruction_sizes[*opcode];

    return 0;
}

#if DEBUG

char* itoa(int val, int base)
{
    
    static char buf[32] = {0};
    
    int i = 30;
    
    for(; val && i ; --i, val /= base)
    
        buf[i] = "0123456789abcdef"[val % base];
    
    return &buf[i+1];
    
}


void print_registers(registers *reg)
{

    printf("A: %#04x | B: %#04x | C: %#04x | D: %#04x | E: %#04x | H: %#04x | L: %#04x \nPC: %#06x | SP: %#06x \nFLAGS: %s\n", 
            reg->A,
            reg->B,
            reg->C,
            reg->D,
            reg->E,
            reg->H,
            reg->L,
            reg->PC,
            reg->SP,
            (reg->FLAGS != 0 ? itoa(reg->FLAGS, 2) : "00000000"));
}


#endif



int main(int argc, char **argv)
{

    if(argc != 2) {printf("Usage: ./8080emu <rom filename>\n"); return 1;}

    computer *COMP = calloc(1, sizeof(computer));
    
    COMP->reg = calloc(1, sizeof(registers));
	COMP->IO =  calloc(1, sizeof(IO))		;

    COMP->mem = malloc(0x10000);  /* 16K memory*/ 


    read_file_to_mem(COMP->mem, argv[1], 0x0000);
    
    int e = 0;

    while(!e) e = emulate_instruction(COMP);
    
    #if DEBUG
        printf("\n");
        print_registers(COMP->reg);
    #endif

    free(COMP);

    return 0;
}

