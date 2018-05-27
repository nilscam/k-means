##
## EPITECH PROJECT, 2018
## FUN - deBruijn
## File description:
## Makefile
##

STACK	=	stack

NAME	=	imageCompressor

RM	=	rm -f

all:		$(NAME)

$(NAME):
		$(STACK) build
		mv `$(STACK) path --local-install-root`/bin/hpack-exe ./
		mv hpack-exe imageCompressor

clean:
		$(STACK) clean
		$(RM) *~
		$(RM) \#*\#

fclean:		clean
		$(RM) $(NAME)

re:		fclean all

.PHONY:		all clean fclean re

